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

#include "FluxusMain.h"
#include "Unicode.h"

#ifndef __APPLE__
#include <GL/glut.h>
#else
#include <GLUT/glut.h>
#endif
#include <iostream>

using namespace std;
using namespace fluxus;

////////////////////////////////////////////////////////////////
// despite attempts at clearing all this up, this
// is still an area of awkward code...

FluxusMain::FluxusMain(int x, int y) :
m_OrigWidth(x),
m_OrigHeight(y),
m_CurrentEditor(9),
m_Width(x),
m_Height(y),
m_HideScript(false),
m_ShowCursor(true),
m_ShowFileDialog(false)
{
	// use the interpreter to get the font name
	// and editor prefs
	Scheme_Object *txt;
	Scheme_Object *t;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, txt);
	MZ_GC_VAR_IN_REG(1, t);
	MZ_GC_REG();
	Interpreter::Interpret(L"fluxus-scratchpad-font", &txt);
	char *s=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(txt),SCHEME_CHAR_STRLEN_VAL(txt),NULL,0);
	Interpreter::Interpret(L"fluxus-scratchpad-do-autofocus", &t);
	GLEditor::m_DoAutoFocus=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-debug-autofocus", &t);
	GLEditor::m_DebugAutoFocus=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-autofocus-width", &t);
	GLEditor::m_AutoFocusWidth=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-autofocus-height", &t);
	GLEditor::m_AutoFocusHeight=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-autofocus-error", &t);
	GLEditor::m_AutoFocusError=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-autofocus-drift", &t);
	GLEditor::m_AutoFocusDrift=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-autofocus-scale-drift", &t);
	GLEditor::m_AutoFocusScaleDrift=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-autofocus-min-scale", &t);
	GLEditor::m_AutoFocusMinScale=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-autofocus-max-scale", &t);
	GLEditor::m_AutoFocusMaxScale=scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-visible-lines", &t);
	GLEditor::m_VisibleLines=(int)scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-visible-columns", &t);
	GLEditor::m_VisibleColumns=(int)scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-x-pos", &t);
	GLEditor::m_XPos=(int)scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-y-pos", &t);
	GLEditor::m_YPos=(int)scheme_real_to_double(t);
	Interpreter::Interpret(L"fluxus-scratchpad-hide-script", &t);
	m_HideScript=SCHEME_TRUEP(t);

	float colour[]={0, 0, 0, 1};
	Interpreter::Interpret(L"fluxus-scratchpad-cursor-colour", &t);
	int ts = SCHEME_VEC_SIZE(t);

	for (int n=0; n<min(4, ts); n++)
	{
		colour[n]=scheme_real_to_double(SCHEME_VEC_ELS(t)[n]);
	}
	GLEditor::m_CursorColourRed=colour[0];
	GLEditor::m_CursorColourGreen=colour[1];
	GLEditor::m_CursorColourBlue=colour[2];
	GLEditor::m_CursorColourAlpha=colour[3];

	MZ_GC_UNREG();

	GLEditor::InitFont(string_to_wstring(string(s)));
	m_FileDialog = new GLFileDialog;

	for(int i=0; i<9; i++)
	{
		m_Editor[i] = new GLEditor();
	}
	Repl *repl = new Repl();
	m_Editor[9] = repl;

	// register the repl with the interpreter so we can
	// use it to output error and output messages
	Interpreter::SetRepl(repl);
}

FluxusMain::~FluxusMain()
{
	for(int i=0; i<10; i++)
	{
		delete m_Editor[i];
	}
}

void FluxusMain::Handle(unsigned char key, int button, int special, int state, int x, int y, int mod)
{
	if (mod&GLUT_ACTIVE_CTRL)
	{
		//for testing
		//cout << "KEY:" << key << " / " << state << endl;

		// pretty sure this is going to have to change...
		switch(key)
		{
			case 6: glutFullScreen(); break; // f
			case 23: // w
			{
				glutReshapeWindow(m_OrigWidth,m_OrigHeight);
				glutPositionWindow(100,100);
			}
			break;
			case 16: Pretty(); break; // p
			case 19: if (m_CurrentEditor!=9) SaveScript(); break; // s
			case 8: HideScript(); break; // h
			case 13: HideCursor(); break; // m
			case 12:
				if (m_CurrentEditor!=9) // don't go into the dialogs from the repl
				{
					m_FileDialog->SetSaveAsMode(false);
					m_FileDialog->Refresh();
					m_ShowFileDialog=!m_ShowFileDialog;
				}
			break; // l
			case 4: // d
				if (m_CurrentEditor!=9) // don't go into the dialogs from the repl
				{
					m_FileDialog->SetSaveAsMode(true);
					m_ShowFileDialog=!m_ShowFileDialog;
				}
			break; // l
			case 2: // b
						m_Editor[m_CurrentEditor]->BlowupCursor();
                        break;

			case 5: // e
			{
				if (m_CurrentEditor!=9)
				{
					Execute();
				}
			}
			break;
            case 18: // r
                // switch to repl (for windows version where ctrl-num is undetectable)
                SetCurrentEditor(9);
            break;
#ifndef __APPLE__
			case 49: SetCurrentEditor(0); break; // 1
			case 0: SetCurrentEditor(1); break; // 2
			case 27: SetCurrentEditor(2); break; // 3
			case 28: SetCurrentEditor(3); break; // 4
			case 29: SetCurrentEditor(4); break; // 5
			case 30: SetCurrentEditor(5); break; // 6
			case 31: SetCurrentEditor(6); break; // 7
			case 127: SetCurrentEditor(7); break; // 8
			case 57: SetCurrentEditor(8); break; // 9
			case 48: SetCurrentEditor(9); break; // 0
#else
			case 49: SetCurrentEditor(0); break; // 1
			case 50: SetCurrentEditor(1); break; // 2
			case 51: SetCurrentEditor(2); break; // 3
			case 52: SetCurrentEditor(3); break; // 4
			case 53: SetCurrentEditor(4); break; // 5
			case 54: SetCurrentEditor(5); break; // 6
			case 55: SetCurrentEditor(6); break; // 7
			case 56: SetCurrentEditor(7); break; // 8
			case 57: SetCurrentEditor(8); break; // 9
			case 48: SetCurrentEditor(9); break; // 0
#endif
			default: break;
		}
	}

	if (key!=0 || special!=-1)
	{
		if (special==GLUT_KEY_F9)
		{
			m_Editor[m_CurrentEditor]->m_DoEffects =
				!m_Editor[m_CurrentEditor]->m_DoEffects;
		}
		else if (special==GLUT_KEY_F10)
		{
			m_Editor[m_CurrentEditor]->m_Alpha-=0.05;
			if (m_Editor[m_CurrentEditor]->m_Alpha<0)
			{
				m_Editor[m_CurrentEditor]->m_Alpha=0;
			}
		}
		else if (special==GLUT_KEY_F11)
		{
			m_Editor[m_CurrentEditor]->m_Alpha+=0.05;
			if (m_Editor[m_CurrentEditor]->m_Alpha>1)
			{
				m_Editor[m_CurrentEditor]->m_Alpha=1;
			}
		}
		else if (special==GLUT_KEY_F4 && m_CurrentEditor<9)
		{
			m_Script=m_Editor[m_CurrentEditor]->GetSExpr();
		}
		else if (special==GLUT_KEY_F5 && m_CurrentEditor<9)
		{
			Execute();
		}
		else if (special==GLUT_KEY_F6 && m_CurrentEditor<9)
		{
			Interpreter::Initialise();
			m_Script=m_Editor[m_CurrentEditor]->GetText();
			SaveBackupScript();
		}

		// the editors only take keyboard events
		if (m_ShowFileDialog)
		{
			if (key==27) // escape
			{
				m_ShowFileDialog=false;
			}

			m_FileDialog->Handle(button,key,special,state,x,y,mod);
			if (m_FileDialog->GetOutput()!=L"")
			{
				if (m_FileDialog->GetSaveAsMode())
				{
					m_SaveName[m_CurrentEditor]=m_FileDialog->GetOutput();
					SaveScript();
				}
				else
				{
					LoadScript(m_FileDialog->GetOutput());
				}
				m_FileDialog->Clear();
				m_ShowFileDialog=false;
			}
		}
		else if (!m_HideScript)
		{
			m_Editor[m_CurrentEditor]->Handle(button,key,special,state,x,y,mod);
		}
	}
}

void FluxusMain::Reshape(int width, int height)
{
	for(int n=0; n<NUM_EDITORS; n++)
	{
		m_Editor[n]->Reshape(width,height);
	}

	m_FileDialog->Reshape(width,height);
	m_Width=width;
	m_Height=height;
}

void FluxusMain::Render()
{
	if (m_ShowFileDialog) m_FileDialog->Render();
	else if (!m_HideScript) m_Editor[m_CurrentEditor]->Render();
}

void FluxusMain::LoadScript(const wstring &Filename)
{
	FILE *file=fopen(wstring_to_string(Filename).c_str(),"r");
	if (file)
	{
		fseek(file,0,SEEK_END);
		long size=ftell(file);
		fseek(file,0,SEEK_SET);

		if (size==0)
		{
			fclose(file);
#ifndef WIN32 // the mingw compiler is broken in regard to unicode
			wcerr<<L"empty file: "<<Filename<<endl;
#else
			cerr<<"empty file: "<<wstring_to_string(Filename)<<endl;
#endif
			return;
		}

		if (size<0)
		{
		  fclose(file);
#ifndef WIN32
			wcerr<<L"error loading file: "<<Filename<<L" size: "<<size<<L"??"<<endl;
#else
			cerr<<"error loading file: "<<wstring_to_string(Filename)<<" size: "<<size<<"??"<<endl;
#endif
			return;
		}

		char *buffer = new char[size+1];
		if (buffer)
		{
            long s = (long)fread(buffer,1,size,file);
			if (size!=s)
			{
#ifndef WIN32 // files seem to shrink with mingw - something to 
              // do with carriage returns being converted
				delete[] buffer;
				fclose(file);
				wcerr<<L"read error: "<<Filename<<endl;
				return;
#endif
			}
			buffer[s]='\0';
			m_Editor[m_CurrentEditor]->SetText(string_to_wstring(string(buffer)));
		}
		else
		{
			cerr<<"couldn't allocate buffer for load"<<endl;
		}

		delete[] buffer;
		fclose(file);
	}
	else
	{
#ifndef WIN32
		wcerr<<L"couldn't load: "<<Filename<<endl;
#else
		cerr<<"couldn't load: "<<wstring_to_string(Filename)<<endl;
#endif
	}

	m_SaveName[m_CurrentEditor]=Filename; // just a precaution
}

void FluxusMain::Execute()
{
	m_Script=m_Editor[m_CurrentEditor]->GetText();
	SaveBackupScript();
}

void FluxusMain::SaveBackupScript() 
{ 
	if (m_SaveName[m_CurrentEditor]==L"") 
	{	
		m_SaveName[m_CurrentEditor]=L"temp.scm";
	}
	
	FILE *file=fopen(wstring_to_string(m_SaveName[m_CurrentEditor]+wstring(L"~")).c_str(),"w");
	if (file)
	{	
        string out = wstring_to_string(m_Editor[m_CurrentEditor]->GetAllText());
		fwrite(out.c_str(),1,out.size(),file);	
		fclose(file);
	}
}

void FluxusMain::SaveScript()
{
	if (m_SaveName[m_CurrentEditor]==L"")
	{
		m_SaveName[m_CurrentEditor]=L"temp.scm";
	}

	FILE *file=fopen(wstring_to_string(m_SaveName[m_CurrentEditor]).c_str(),"w");
	if (file)
	{
        string out = wstring_to_string(m_Editor[m_CurrentEditor]->GetAllText());
		fwrite(out.c_str(),1,out.size(),file);
		fclose(file);
#ifndef WIN32 // the mingw compiler doesn't really do much unicode stuff
		wcerr<<L"Saved ["<<m_SaveName[m_CurrentEditor]<<L"]"<<endl;
#else
		cerr<<"Saved ["<<wstring_to_string(m_SaveName[m_CurrentEditor])<<"]"<<endl;		
#endif
	}
	else
	{
#ifndef WIN32
		wcerr<<L"Could not save file ["<<m_SaveName[m_CurrentEditor]<<L"]"<<endl;
#else
		cerr<<L"Could not save file ["<<wstring_to_string(m_SaveName[m_CurrentEditor])<<L"]"<<endl;
#endif
	}
}

wstring Escape(const wstring &str)
{
	wstring out;
	for(unsigned int i=0; i<str.size(); i++)
	{
		if (str[i]==L'\"') out+=L"\\\"";
		else if (str[i]==L'\\') out+=L"\\\\";
		else out+=str[i];
	}
	return out;
}

void FluxusMain::Pretty()
{
	Scheme_Object *txt;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, txt);
	MZ_GC_REG();	
	wstring command(wstring(L"(fluxus-auto-indent \"")+Escape(m_Editor[m_CurrentEditor]->GetAllText())+wstring(L"\")"));
	Interpreter::Interpret(command, &txt);
	if (txt!=NULL)
	{
		char *s=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(txt),SCHEME_CHAR_STRLEN_VAL(txt),NULL,0);
		m_Editor[m_CurrentEditor]->SetText(string_to_wstring(string(s)));	
	}
  	MZ_GC_UNREG();
}

void FluxusMain::HideCursor()
{
	m_ShowCursor=!m_ShowCursor;
	if (m_ShowCursor)
		glutSetCursor(GLUT_CURSOR_INHERIT);
	else
		glutSetCursor(GLUT_CURSOR_NONE);
}

