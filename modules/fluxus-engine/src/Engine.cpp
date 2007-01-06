// Copyright (C) 2007 Dave Griffiths
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

#include <assert.h>
#include <plt/escheme.h>
#include "Engine.h"
#include "GraphicsUtils.h"

using namespace fluxus;

Engine *Engine::m_Engine=NULL;

PolyPrimitive*  Engine::StaticCube=NULL;
PolyPrimitive*  Engine::StaticPlane=NULL;
PolyPrimitive*  Engine::StaticSphere=NULL;
PolyPrimitive*  Engine::StaticCylinder=NULL;

Engine::Engine()
{
	StaticCube = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(StaticCube);

	StaticPlane = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(StaticPlane);

	StaticSphere = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(StaticSphere,1,5,10);

	StaticCylinder = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(StaticCylinder,1,1,5,10);

	ResetRenderers();	
}

unsigned int Engine::MakeRenderer()
{
	fluxus::Renderer *r = new fluxus::Renderer;
	fluxus::Physics *p = new fluxus::Physics(r);
	m_RendererVec.push_back(pair<fluxus::Renderer*, fluxus::Physics*>(r,p));
	return m_RendererVec.size()-1;
}

bool Engine::PushRenderer(unsigned int id)
{
	if (id<m_RendererVec.size())
	{
		m_RendererStack.push_back(id);
		ClearGrabStack();
		return true;
	}
	return false;
}

void Engine::PopRenderer()
{
	// make sure we don't pop the default renderer
	if (m_RendererVec.size()>1)
	{
		m_RendererStack.pop_back();
		ClearGrabStack();
	}
}

Renderer *Engine::Renderer()
{
	// the renderer stack should never be empty
	assert(!m_RendererStack.empty());
	// and this should have been checked by push renderer
	assert(*m_RendererStack.rbegin()<m_RendererVec.size());
	return m_RendererVec[*m_RendererStack.rbegin()].first;
}

Physics *Engine::Physics()
{
	// the renderer stack should never be empty
	assert(!m_RendererStack.empty());
	// and this should have been checked by push renderer
	assert(*m_RendererStack.rbegin()<m_RendererVec.size());
	return m_RendererVec[*m_RendererStack.rbegin()].second;
}
void Engine::BeginScene()
{
	Renderer()->BeginScene();
}

void Engine::EndScene()
{
	Renderer()->EndScene();
}

void Engine::ResetRenderers()
{
	for (vector<pair<fluxus::Renderer *, fluxus::Physics *> >::iterator i=m_RendererVec.begin(); 
		i!=m_RendererVec.end(); i++)
	{
		delete i->first;
		delete i->second;
	}
	
	m_RendererVec.clear();
	m_RendererStack.clear();
	
	// make the default renderer
	PushRenderer(MakeRenderer());
	ClearGrabStack();
}

// todo - move the grabstack into the renderer?
void Engine::PushGrab(int id)
{	
	m_GrabStack.push_front(id);
	m_Grabbed=Renderer()->GetPrimitive(id);
	Renderer()->Grab(id);
}

void Engine::PopGrab()
{
	Renderer()->UnGrab();
	m_Grabbed=NULL;
	
	if (!m_GrabStack.empty())
	{
		m_GrabStack.pop_front();
		if (!m_GrabStack.empty())
		{
			m_Grabbed=Renderer()->GetPrimitive(*m_GrabStack.begin());
			Renderer()->Grab(*m_GrabStack.begin());
		}
	}
}

void Engine::ClearGrabStack()
{
	m_GrabStack.clear();
	m_Grabbed=NULL;
}

fluxus::State *Engine::State()
{
    if (Grabbed()) 
	{
		return Grabbed()->GetState();
	}
	return Renderer()->GetState();
}

