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

#ifndef _FLUXUS_INTERPRETER_H_
#define _FLUXUS_INTERPRETER_H_

#include <deque>
#include <string>
#include <plt/scheme.h>

namespace fluxus 
{

class Repl;

class Interpreter 
{
public:
	Interpreter(Scheme_Env *e);
	~Interpreter();
	
	void SetRepl(Repl *s) { m_Repl=s; }
	
	Scheme_Object *Interpret(const std::string &code, bool abort=false);
	
private:
	Scheme_Env *m_Scheme;
	Repl *m_Repl;

};

}

#endif
