// Copyright (C) 2008 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 
#include "assert.h"
#include "PolyPrimitive.h"
#include "OBJPrimitiveIO.h"
#include "Trace.h"

using namespace Fluxus;
	
OBJPrimitiveIO::OBJPrimitiveIO()
{
}

OBJPrimitiveIO::~OBJPrimitiveIO()
{
	// clear some mem
	m_Position.clear();
	m_Texture.clear();
	m_Normal.clear();
	m_Faces.clear();
}
	
Primitive *OBJPrimitiveIO::FormatRead(const string &filename)
{
	FILE *file = fopen(filename.c_str(),"r");
	if (file==NULL)
	{
		Trace::Stream<<"Cannot open .obj file: "<<filename<<endl;
		return NULL;
	}
	
	fseek(file,0,SEEK_END);
	m_DataSize = ftell(file);
	rewind(file);
	
	m_Data = new char[m_DataSize+1];
	if (m_DataSize!=fread(m_Data,1,m_DataSize,file))
	{
		Trace::Stream<<"Error reading .obj file: "<<filename<<endl;
		fclose(file);
		return NULL;
	}
  	fclose(file);
	m_Data[m_DataSize]='\0';
    
	// get all the data we need
  	ReadVectors("v",m_Position);
  	ReadVectors("vt",m_Texture);
  	ReadVectors("vn",m_Normal);
	ReadIndices(m_Faces);
	
	// now get rid of the text
	delete[] m_Data;
	
	// shuffle stuff around so we only have one set of indices
	vector<Indices> unique=RemoveDuplicateIndices();
	ReorderData(unique);
	UnifyIndices(unique);
	
	if (m_Faces.empty()) return NULL;
	
	return MakePrimitive();
}

Primitive *OBJPrimitiveIO::MakePrimitive()
{
	// stick all the data in a primitive
	
	// what type?
	PolyPrimitive::Type type;
	switch (m_Faces[0].Index.size())
	{
		case 3: type=PolyPrimitive::TRILIST; break;
		case 4: type=PolyPrimitive::QUADS; break;
		default: 
		{
			Trace::Stream<<"obj file needs to contain triangles or quads"<<endl;
			return NULL;
		}
	}
	
	PolyPrimitive *prim = new PolyPrimitive(type);
	prim->Resize(m_Position.size());
	
	TypedPData<dVector> *pos = new TypedPData<dVector>(m_Position);
	prim->SetDataRaw("p", pos);
	
	if (!m_Texture.empty())
	{
		assert(m_Texture.size()==m_Position.size());
		TypedPData<dVector> *tex = new TypedPData<dVector>(m_Texture);
		prim->SetDataRaw("t", tex);
	}
	
	if (!m_Normal.empty())
	{	
		assert(m_Normal.size()==m_Position.size());
		TypedPData<dVector> *nrm = new TypedPData<dVector>(m_Normal);
		prim->SetDataRaw("n", nrm);
	}

	prim->GetIndex()=m_Indices;
	prim->SetIndexMode(true);
	return prim;
}

unsigned int OBJPrimitiveIO::TokeniseLine(unsigned int pos, vector<string> &output)
{
	char c=m_Data[pos];
	vector<string> temp;
	temp.push_back("");
	while(c!='\n' && pos<m_DataSize)
	{
		if (c==' ' && *temp.rbegin()!="") temp.push_back("");
		else temp.rbegin()->push_back(c);
		c=m_Data[++pos];
	}		
	
	// get rid of whitespace
	output.clear();
	for(vector<string>::iterator i=temp.begin(); i!=temp.end(); ++i)
	{
		if (*i!="")	output.push_back(*i);
	}
	
	return pos+1;
}

void OBJPrimitiveIO::TokeniseIndices(const string &str, vector<string> &output)
{
	unsigned int pos=0;
	output.clear();
	output.push_back("");
	while(pos<str.size())
	{
		char c=str[pos++];
		if (c==' ' || c=='/') output.push_back("");
		else output.rbegin()->push_back(c);
	}			
}

void OBJPrimitiveIO::ReadVectors(const string &code, std::vector<dVector> &output)
{
	unsigned int pos=0;
	output.clear();
	while (pos<m_DataSize)
	{
		vector<string> tokens;
		pos = TokeniseLine(pos, tokens);
		if (tokens.size()==4 && tokens[0]==code)
		{
			output.push_back(dVector(atof(tokens[1].c_str()),
			                         atof(tokens[2].c_str()),
									 atof(tokens[3].c_str())));
		}
	}
}

void OBJPrimitiveIO::ReadIndices(vector<Face> &output)
{
	unsigned int pos=0;
	output.clear();
	while (pos<m_DataSize)
	{
		vector<string> tokens;
		pos = TokeniseLine(pos, tokens);
		if (!tokens.empty() && tokens[0]=="f")
		{
			Face f;
			for(unsigned int i=1; i<tokens.size(); i++)
			{
				vector<string> itokens;
				TokeniseIndices(tokens[i],itokens);
				if (itokens.size()==3)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					if (itokens[1]!="") ind.Texture=(unsigned int)atof(itokens[1].c_str())-1;
					if (itokens[2]!="") ind.Normal=(unsigned int)atof(itokens[2].c_str())-1;
					f.Index.push_back(ind);
				}
				else if (itokens.size()==2)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					if (itokens[1]!="") ind.Texture=(unsigned int)atof(itokens[1].c_str())-1;
					f.Index.push_back(ind);
				}
				else
				{
					Trace::Stream<<"Wrong number of indices in .obj file ("<<itokens.size()<<")"<<endl;
				}
			}
			output.push_back(f);
		}
	}
}

vector<OBJPrimitiveIO::Indices> OBJPrimitiveIO::RemoveDuplicateIndices()
{
	vector<Indices> ret;
	for (vector<Face>::const_iterator fi=m_Faces.begin();
		fi!=m_Faces.end(); ++fi)
	{
		for (vector<Indices>::const_iterator ii=fi->Index.begin();
			ii!=fi->Index.end(); ++ii)
		{
			bool exists=false;
			for (vector<Indices>::iterator ri=ret.begin();
				ri!=ret.end(); ++ri)
			{	
				if (*ri==*ii) 
				{
					exists=true;
					break;
				}
			}
			if (!exists) ret.push_back(*ii);
		}
	}
	return ret;
}

void OBJPrimitiveIO::ReorderData(const vector<OBJPrimitiveIO::Indices> &unique)
{
	vector<dVector> NewPosition;
	vector<dVector> NewTexture;
	vector<dVector> NewNormal;
	
	for (vector<Indices>::const_iterator i=unique.begin();
		i!=unique.end(); ++i)
	{
		if (!m_Position.empty()) NewPosition.push_back(m_Position[i->Position]);
		if (!m_Texture.empty()) NewTexture.push_back(m_Texture[i->Texture]);
		if (!m_Normal.empty()) NewNormal.push_back(m_Normal[i->Normal]);
	}

	m_Position=NewPosition;
	m_Texture=NewTexture;
	m_Normal=NewNormal;
}

void OBJPrimitiveIO::UnifyIndices(const vector<Indices> &unique)
{
	m_Indices.clear();
	for (vector<Face>::const_iterator fi=m_Faces.begin();
		fi!=m_Faces.end(); ++fi)
	{
		for (vector<Indices>::const_iterator ii=fi->Index.begin();
			ii!=fi->Index.end(); ++ii)
		{
			unsigned int index=0;
			for (vector<Indices>::const_iterator ri=unique.begin();
				ri!=unique.end(); ++ri)
			{	
				if (*ri==*ii) break;				
				index++;
			}
			m_Indices.push_back((unsigned int)index);
		}
	}
}

//////////////////////////////////

void OBJPrimitiveIO::WriteVertices(const string &pdataname, const string &objname, const Primitive *ob, FILE *file)
{
	char line[2048];
	const TypedPData<dVector> *pdata = dynamic_cast<const TypedPData<dVector> *>(ob->GetDataRawConst(pdataname));
	for (unsigned int i=0; i<ob->Size(); i++)
	{
		dVector o = pdata->m_Data[i];
		snprintf(line,2048,"%s %f %f %f\n",objname.c_str(),o.x,o.y,o.z);
		fwrite(line,1,strlen(line),file);
	}
}

void OBJPrimitiveIO::WriteIndices(const Primitive *ob, FILE *file)
{
	char line[2048];
	const PolyPrimitive *pp = dynamic_cast<const PolyPrimitive *>(ob);
	
	int facecount=3;
	switch (pp->GetType())
	{
		case PolyPrimitive::TRILIST: facecount=3; break;
		case PolyPrimitive::QUADS: facecount=4; break;
		default: 
		{
			Trace::Stream<<"primitive can only be saved with type triangle-list or quad-list"<<endl;
			return;
		}
	}
	
	if (pp->IsIndexed())
	{
		vector<unsigned int> indices = pp->GetIndexConst();
		unsigned int i=0;
		while (i<pp->Size())
		{
			snprintf(line,2048,"f ");
			fwrite(line,1,strlen(line),file);

			for (int c=0; c<facecount; c++)
			{
				snprintf(line,2048,"%d/%d/%d ",indices[i]+1,indices[i]+1,indices[i]+1);
				fwrite(line,1,strlen(line),file);
				i++;
			}
			
			snprintf(line,2048,"\n");
			fwrite(line,1,strlen(line),file);
		}
	}
	else
	{
		unsigned int i=0;
		while (i<pp->Size())
		{
			snprintf(line,2048,"f ");
			fwrite(line,1,strlen(line),file);

			for (int c=0; c<facecount; c++)
			{
				snprintf(line,2048,"%d/%d/%d ",i+1,i+1,i+1);
				fwrite(line,1,strlen(line),file);
				i++;
			}
			
			snprintf(line,2048,"\n");
			fwrite(line,1,strlen(line),file);
		}
	}
}

bool OBJPrimitiveIO::FormatWrite(const std::string &filename, const Primitive *ob)
{
	FILE *file = fopen(filename.c_str(),"w");
	if (file==NULL)
	{
		Trace::Stream<<"Cannot open .obj file: "<<filename<<endl;
		return false;
	}
	
	WriteVertices("p","v",ob,file);
	WriteVertices("n","vn",ob,file);
	WriteVertices("t","vt",ob,file);
	WriteIndices(ob,file);
	
	fclose(file);
	
	return false;
}
	