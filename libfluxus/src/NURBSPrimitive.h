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
#include "Primitive.h"

#ifndef N_NURBSPRIM
#define N_NURBSPRIM

namespace fluxus
{

class NURBSPrimitive : public Primitive
{
public:	
	NURBSPrimitive();
	virtual  ~NURBSPrimitive();
	
	virtual void Render();
	virtual void RecalculateNormals(bool smooth);
	virtual dBoundingBox GetBoundingBox();
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "NURBSPrimitive"; }

	/// Sets the order of the patches - call this first
	void Init(int orderu, int orderv, int ucvs, int vcvs) { m_UOrder=orderu; m_VOrder=orderv; m_UCVCount=ucvs; m_VCVCount=vcvs; }
	void AddCV(const dVector &CV) { m_CVVec->push_back(CV); }	
	void AddN(const dVector &N) { m_NVec->push_back(N); }	
	void AddTex(const dVector &ST) { m_STVec->push_back(ST); }	
	void AddUKnot(float k) { m_UKnotVec.push_back(k); }	
	void AddVKnot(float k) { m_VKnotVec.push_back(k); }	

	
protected:
	
	virtual void PDataDirty();
	
	vector<dVector> *m_CVVec;
	vector<dVector> *m_STVec;
	vector<dVector> *m_NVec;
	vector<float> m_UKnotVec;
	vector<float> m_VKnotVec;
	
	int m_UOrder;
	int m_VOrder;
	int m_UCVCount;
	int m_VCVCount;
	int m_Stride;
	
	GLUnurbsObj *m_Surface;
};

};

#endif
