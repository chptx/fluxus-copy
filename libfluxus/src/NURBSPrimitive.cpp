// Copyright (C) 2005 Dave Griffiths
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

#include "Renderer.h"
#include "NURBSPrimitive.h"
#include "State.h"

using namespace Fluxus;

NURBSPrimitive::NURBSPrimitive() :
m_UOrder(0),
m_VOrder(0),
m_UCVCount(0),
m_VCVCount(0),
m_Stride(sizeof(dVector)/sizeof(float))
{
	AddData("p",new TypedPData<dVector>);
	AddData("t",new TypedPData<dVector>);
	AddData("n",new TypedPData<dVector>);
	AddData("c",new TypedPData<dColour>);

	// direct access for speed
	PDataDirty();

	SetupSurface();
}

NURBSPrimitive::NURBSPrimitive(const NURBSPrimitive &other) :
Primitive(other),
m_UKnotVec(other.m_UKnotVec),
m_VKnotVec(other.m_VKnotVec),
m_UOrder(other.m_UOrder),
m_VOrder(other.m_VOrder),
m_UCVCount(other.m_UCVCount),
m_VCVCount(other.m_VCVCount),
m_Stride(other.m_Stride)
{
	SetupSurface();
	PDataDirty();
}

NURBSPrimitive::~NURBSPrimitive()
{
	gluDeleteNurbsRenderer(m_Surface);
}

NURBSPrimitive* NURBSPrimitive::Clone() const
{
	return new NURBSPrimitive(*this);
}

void NURBSPrimitive::PDataDirty()
{
	// reset pointers
	m_CVVec=GetDataVec<dVector>("p");
	m_STVec=GetDataVec<dVector>("t");
	m_NVec=GetDataVec<dVector>("n");
	m_ColData=GetDataVec<dColour>("c");
}

void NURBSPrimitive::SetupSurface()
{
	m_Surface = gluNewNurbsRenderer();
	//gluNurbsProperty(m_Surface, GLU_SAMPLING_METHOD, GLU_PARAMETRIC_ERROR);
	//gluNurbsProperty(m_Surface, GLU_PARAMETRIC_TOLERANCE, 5.0);
	gluNurbsProperty(m_Surface, GLU_SAMPLING_METHOD, GLU_DOMAIN_DISTANCE);
	gluNurbsProperty(m_Surface, GLU_U_STEP, 20);
	gluNurbsProperty(m_Surface, GLU_DISPLAY_MODE, GLU_FILL);
	gluNurbsProperty(m_Surface, GLU_CULLING, GLU_TRUE);
}

void NURBSPrimitive::Render()
{
	if (m_State.Hints & HINT_UNLIT) glDisable(GL_LIGHTING);

	if (m_State.Hints & HINT_AALIAS) glEnable(GL_LINE_SMOOTH);
	else glDisable(GL_LINE_SMOOTH);

	if (m_State.Hints & HINT_SOLID)
	{
		gluNurbsProperty(m_Surface, GLU_DISPLAY_MODE, GLU_FILL);

		gluBeginSurface(m_Surface);

		if (!m_STVec->empty())
		{
			gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),m_VKnotVec.size(),&(*m_VKnotVec.begin()),
									 m_VCVCount*m_Stride,m_Stride,
									 m_STVec->begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_TEXTURE_COORD_2);
		}

		if (!m_NVec->empty())
		{
			gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),m_VKnotVec.size(),&(*m_VKnotVec.begin()),
									 m_VCVCount*m_Stride,m_Stride,
									 m_NVec->begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_NORMAL);
		}

		gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),m_VKnotVec.size(),&(*m_VKnotVec.begin()),
								 m_VCVCount*m_Stride,m_Stride,
								 m_CVVec->begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_VERTEX_3);

		if (m_State.Hints & HINT_VERTCOLS)
		{
			gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),
					m_VKnotVec.size(),&(*m_VKnotVec.begin()),
					m_VCVCount*m_Stride,m_Stride,
					m_ColData->begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_COLOR_4);
		}

		gluEndSurface(m_Surface);
	}

	if (m_State.Hints & HINT_WIRE)
	{
		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glEnable(GL_LINE_STIPPLE);
			glLineStipple(m_State.StippleFactor, m_State.StipplePattern);
		}
		glDisable(GL_LIGHTING);
		glColor4fv(m_State.WireColour.arr());
		gluNurbsProperty(m_Surface, GLU_DISPLAY_MODE, GLU_OUTLINE_POLYGON);

		/* glPolygonMode is changed from the default GL_FILL to GL_LINE
		 * after the gluNurbsSurface call on OSX 10.5.7,
		 * see: https://savannah.nongnu.org/bugs/?26951
		 */
#ifdef __APPLE__
		glPushAttrib(GL_POLYGON_BIT); // save GL_POLYGON_MODE
#endif
		gluBeginSurface(m_Surface);
		gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),m_VKnotVec.size(),&(*m_VKnotVec.begin()),
						m_VCVCount*m_Stride,m_Stride,
						m_CVVec->begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_VERTEX_3);
		gluEndSurface(m_Surface);
#ifdef __APPLE__
		glPopAttrib(); // restore the original GL_POLYGON_MODE
#endif

		glEnable(GL_LIGHTING);
		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glDisable(GL_LINE_STIPPLE);
		}
	}

	if (m_State.Hints & HINT_POINTS)
	{
		glColor3f(0,0,1);
		glDisable(GL_LIGHTING);
		glBegin(GL_POINTS);
		for (unsigned int n=0; n<m_CVVec->size(); n++)
		{
			glVertex3fv((*m_CVVec)[n].arr());
		}
		glEnd();
		glEnable(GL_LIGHTING);
	}

	if (m_State.Hints & HINT_NORMAL)
	{
		glColor3f(1,0,0);
		glDisable(GL_LIGHTING);
		glBegin(GL_LINES);
		for (unsigned int i=0; i!=m_CVVec->size(); i++)
		{
			glVertex3fv((*m_CVVec)[i].arr());
			glVertex3fv(((*m_CVVec)[i]+(*m_NVec)[i]).arr());
		}
		glEnd();
		glEnable(GL_LIGHTING);
	}

	if (m_State.Hints & HINT_UNLIT) glEnable(GL_LIGHTING);
}

void NURBSPrimitive::RecalculateNormals(bool smooth)
{
	for (int n=0; n<(int)m_NVec->size(); n++)
	{
		int u=n-1;
		bool flip=false;
		if (n%m_VCVCount==0)
		{
			u=n+1;
			flip=true;
		}

		int v=n-m_VCVCount;

		if (n<m_VCVCount)
		{
			v=n+m_VCVCount;
			flip=true;
		}

		dVector a=(*m_CVVec)[n]-(*m_CVVec)[u];
		dVector b=(*m_CVVec)[v]-(*m_CVVec)[n];

		a.normalise();
		b.normalise();
		(*m_NVec)[n]=a.cross(b);
		(*m_NVec)[n].normalise();

		if (flip)
		{
			(*m_NVec)[n]=-(*m_NVec)[n];
		}

	}
}

dBoundingBox NURBSPrimitive::GetBoundingBox(const dMatrix &space)
{
	dBoundingBox box;
	for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=m_CVVec->begin();	i!=m_CVVec->end(); ++i)
	{
		box.expand(space.transform(*i));
	}
	return box;
}

void NURBSPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=m_CVVec->begin(); i!=m_CVVec->end(); ++i)
		{
			*i=GetState()->Transform.transform(*i);
		}
	}
	else
	{
		for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=m_CVVec->begin(); i!=m_CVVec->end(); ++i)
		{
			*i=GetState()->Transform.transform_no_trans(*i);
		}
	}

	GetState()->Transform.init();
}

