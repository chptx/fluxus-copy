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
#include "SchemeHelper.h"
#include "Engine.h"
#include "LocalStateFunctions.h"
#include "Renderer.h"

using namespace LocalStateFunctions;
using namespace SchemeHelper;

// StartFunctionDoc
// push
// Description:
// Pushes a copy of the current drawing state to the top of the stack. The drawing state 
// contains information about things like the current colour, transformation and hints.
// Example:
// (colour (vector 1 0 0)) ; set current colour to red
// (push)                  ; copy and push drawing state
// (colour (vector 0 1 0)) ; set current colour to green
// (draw-cube)             ; draws a green cube
// (pop)				   ; forget old drawing state
// ; current colour is now red again
// EndFunctionDoc

Scheme_Object *push(int argc, Scheme_Object **argv)
{
	if (Engine::Get()->Grabbed())
	{
		cerr<<"error: can't (push) while an object is (grab)bed"<<endl;
		return scheme_void;
	}
	
    Engine::Get()->Renderer()->PushState();
    return scheme_void;
}

// StartFunctionDoc
// pop
// Description:
// Destroys the current drawing state, and sets the current one to be the previously pushed 
// one in the stack. The drawing state contains information about things like the current 
// colour, transformation and hints.
// Example:
// (colour (vector 1 0 0)) ; set current colour to red
// (push)                  ; copy and push drawing state
// (colour (vector 0 1 0)) ; set current colour to green
// (draw-cube)             ; draws a green cube
// (pop)				   ; forget old drawing state
// ; current colour is now red again
// EndFunctionDoc

Scheme_Object *pop(int argc, Scheme_Object **argv)
{
	if (Engine::Get()->Grabbed())
	{
		cerr<<"error: can't (pop) while an object is (grab)bed"<<endl;
		return scheme_void;
	}

    Engine::Get()->Renderer()->PopState();
    return scheme_void;
}

// StartFunctionDoc
// grab object-id
// Description:
// Grabs the specified object. Once an object has grabbed it's state can be modified using
// the same commands used to set the current drawing state. (ungrab) needs to be used to
// return to the normal drawing state. Grabbing can also be stacked, in which case ungrab
// pops to the last grabbed primitive.
// Example:
// (colour (vector 1 0 0))      ; set the current colour to red
// (define mycube (build-cube)) ; makes a red cube 
// (grab mycube)  				       
// (colour (vector 0 1 0)) ; sets the cubes colour to green 
// (ungrab)				   ; return to normal state
// EndFunctionDoc

Scheme_Object *grab(int argc, Scheme_Object **argv)
{
	ArgCheck("grab", "i", argc, argv);
	Engine::Get()->PushGrab(IntFromScheme(argv[0]));
	return scheme_void;
}

// StartFunctionDoc
// ungrab
// Description:
// Ungrabs the currently grabbed object, and either returns to the normal drawing state, 
// or pops to the last grabbed primitive. 
// Example:
// (colour (vector 1 0 0))      ; set the current colour to red
// (define mycube (build-cube)) ; makes a red cube 
// (grab mycube)  				       
// (colour (vector 0 1 0)) ; sets the cubes colour to green 
// (ungrab)				   ; return to normal state
// EndFunctionDoc

Scheme_Object *ungrab(int argc, Scheme_Object **argv)
{
	Engine::Get()->PopGrab();
	return scheme_void;
}

// StartFunctionDoc
// apply object-id
// Description:
// Applies the current object transform to the vertex positions of the supplied object and 
// sets it's transform to identity.
// Example:
// (rotate (vector 45 0 0))     
// (define mycube (build-cube)) ; makes a cube with a rotation 
// (apply mycube)  				; applies the rotation to the points of the cube
// EndFunctionDoc

Scheme_Object *apply(int argc, Scheme_Object **argv)
{
	ArgCheck("apply", "i", argc, argv);
	Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]))->ApplyTransform();
	return scheme_void;
}

// StartFunctionDoc
// opacity value
// Description:
// Sets the opacity of the current drawing state, or the currently grabbed primitive.
// Example:
// (opacity 0.5)     
// (define mycube (build-cube)) ; makes a half transparent cube 
// EndFunctionDoc

Scheme_Object *opacity(int argc, Scheme_Object **argv)
{
	ArgCheck("opacity", "f", argc, argv);
    Engine::Get()->State()->Opacity=FloatFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// shinyness value
// Description:
// Sets the shinyness of the current drawing state, or the currently grabbed primitive. 
// This value sets the tightness of the specular highlight.
// Example:
// (shinyness 100)     
// (specular (vector 1 1 1)) ; sets the specular colour
// (define mysphere (build-sphere 10 10)) ; makes a shiny cube 
// EndFunctionDoc

Scheme_Object *shinyness(int argc, Scheme_Object **argv)
{
	ArgCheck("shinyness", "f", argc, argv);
    Engine::Get()->State()->Shinyness=FloatFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// colour colour-vector
// Description:
// Sets the colour of the current drawing state, or the currently grabbed primitive. 
// Example:
// (colour (vector 1 0.5 0.1)) ; mmm orange...   
// (define mycube (build-cube)) ; makes an orange cube 
// EndFunctionDoc

Scheme_Object *colour(int argc, Scheme_Object **argv)
{
	ArgCheck("colour", "v", argc, argv);
    Engine::Get()->State()->Colour=ColourFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// wire-colour colour-vector
// Description:
// Sets the wire frame colour of the current drawing state, or the currently grabbed 
// primitive. Visible with (hint-wire) on most primitives.
// Example:
// (wire-colour (vector 1 1 0)) ; set yellow as current wire colour
// (hint-wire)   
// (define mycube (build-cube)) ; makes a cube with yellow wireframe 
// EndFunctionDoc

Scheme_Object *wire_colour(int argc, Scheme_Object **argv)
{
	ArgCheck("wire-colour", "v", argc, argv);
   	Engine::Get()->State()->WireColour=ColourFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// specular colour-vector
// Description:
// Sets the specular colour of the current drawing state, or the currently grabbed 
// primitive. 
// Example:
// (specular (vector 0 0 1)) ; set blue as specular colour
// (define mysphere (build-sphere 10 10)) ; makes a shiny blue sphere
// EndFunctionDoc

Scheme_Object *specular(int argc, Scheme_Object **argv)
{
	ArgCheck("specular", "v", argc, argv);
    Engine::Get()->State()->Specular=ColourFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// ambient colour-vector
// Description:
// Sets the ambient colour of the current drawing state, or the currently grabbed 
// primitive. 
// Example:
// (ambient (vector 0 0 1)) ; set blue as ambient colour
// (define mysphere (build-sphere 10 10)) ; makes a boringly blue sphere 
// EndFunctionDoc

Scheme_Object *ambient(int argc, Scheme_Object **argv)
{
 	ArgCheck("ambient", "v", argc, argv);
    Engine::Get()->State()->Ambient=ColourFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// opacity value
// Description:
// Sets the emissive colour of the current drawing state, or the currently grabbed 
// primitive. 
// Example:
// (emissive (vector 0 0 1)) ; set blue as emissive colour
// (define mysphere (build-sphere 10 10)) ; makes an bright blue sphere 
// EndFunctionDoc

Scheme_Object *emissive(int argc, Scheme_Object **argv)
{
	ArgCheck("emissive", "v", argc, argv);
    Engine::Get()->State()->Emissive=ColourFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// identity
// Description:
// Sets the drawing state transform to identity, on the state stack, or the currently 
// grabbed primitive. 
// Example:
// (push)
// (scale (vector 2 2 2)) ; set the current scale to double in each dimension
// (define mycube (build-cube)) ; make a scaled cube 
// (pop)
// (grab mycube)
// (identity) ; erases the transform and puts the cube back to its original state
// (ungrab)
// EndFunctionDoc

Scheme_Object *flux_identity(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Transform.init();
    return scheme_void;
}

// StartFunctionDoc
// concat matrix
// Description:
// Concatenates (multiplies) a matrix on to the current drawing state or grabbed primitive. 
// Example:
// (define mymatrix (mrotate (vector 0 45 0))) ; make a matrix
// (concat mymatrix) ; concat it into the current state
// (build-cube) ; make a cube with this rotation
// EndFunctionDoc

Scheme_Object *concat(int argc, Scheme_Object **argv)
{
 	ArgCheck("concat", "m", argc, argv);
	dMatrix m;
	FloatsFromScheme(argv[0],m.arr(),16);
    Engine::Get()->State()->Transform*=m;
    return scheme_void;
}

// StartFunctionDoc
// translate vector
// Description:
// Applies a translation to the current drawing state transform or grabbed primitive.
// Example:
// (transform (vector 0 1.4 0)) ; translates the current transform up a bit
// (build-cube) ; build a cube with this transform
// EndFunctionDoc

Scheme_Object *translate(int argc, Scheme_Object **argv)
{
	ArgCheck("translate", "v", argc, argv);
	dVector t;
	FloatsFromScheme(argv[0],t.arr(),3);
    Engine::Get()->State()->Transform.translate(t.x,t.y,t.z);
    return scheme_void;
}

// StartFunctionDoc
// rotate vector-or-quaternion
// Description:
// Applies a rotation to the current drawing state transform or grabbed primitive.
// Example:
// (rotate (vector 0 45 0)) ; turns 45 degrees in the Y axis
// (build-cube) ; build a cube with this transform
// EndFunctionDoc

Scheme_Object *rotate(int argc, Scheme_Object **argv)
{
  	if (!SCHEME_VECTORP(argv[0])) scheme_wrong_type("rotate", "vector", 0, argc, argv);
	
	if (SCHEME_VEC_SIZE(argv[0])==3)
	{
		// euler angles
		float rot[3];
		FloatsFromScheme(argv[0],rot,3);
	    Engine::Get()->State()->Transform.rotxyz(rot[0],rot[1],rot[2]);
	}
	else if (SCHEME_VEC_SIZE(argv[0])==4)
	{
		// quaternion
		dQuat a;
		FloatsFromScheme(argv[0],a.arr(),4);
		dMatrix m=a.toMatrix();
	 	Engine::Get()->State()->Transform*=m;
	}
	else
	{
		cerr<<"rotate - wrong number of elements in vector"<<endl;
	}
    return scheme_void;
}

// StartFunctionDoc
// scale vector
// Description:
// Applies a scale to the current drawing state transform or grabbed primitive.
// Example:
// (scale (vector 0.5 0.5 0.5)) ; scales the current transform to half the size
// (build-cube) ; build a cube with this transform
// EndFunctionDoc

Scheme_Object *scale(int argc, Scheme_Object **argv)
{
	ArgCheck("scale", "v", argc, argv);
	dVector t;
	FloatsFromScheme(argv[0],t.arr(),3);
    Engine::Get()->State()->Transform.scale(t.x,t.y,t.z);
    return scheme_void;
}

// StartFunctionDoc
// get-transform
// Description:
// Returns a matrix representing the current state transform or for the 
// grabbed primitive.
// Example:
// (translate (vector 1 0 0))
// (display (get-transform))(newline) ; prints the current transform
// (define shape (build-sphere 10 10))
// (grab shape)
// (translate (vector 0 1 0))
// (display (get-transform))(newline) ; prints shape's transform
// (ungrab)
// EndFunctionDoc

Scheme_Object *get_transform(int argc, Scheme_Object **argv)
{
	return FloatsToScheme(Engine::Get()->State()->Transform.arr(),16);
}

// StartFunctionDoc
// parent primitive-id
// Description:
// Parents the currently grabbed primitive to the supplied parent primitive. The current
// primitive will now be moved around with the parent by aquiring all the parent's
// transforms.
// Example:
// (define parent-prim (build-cube)) ; make a parent cube
// (translate (vector 2 0 0)) ; move a bit in x
// (parent parent-prim) ; set parent-prim as the current parent
// (define child-prim (build-cube)) ; make a child cube
// (grab parent-prim) 
// (rotate (vector 0 45 0)) ; the child will now be moved by this transform in addition to its own
// (ungrab)
// EndFunctionDoc

Scheme_Object *parent(int argc, Scheme_Object **argv)
{
	ArgCheck("parent", "i", argc, argv);
    Engine::Get()->State()->Parent=IntFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// line-width value
// Description: 
// Sets the line width (in screen space) of the current drawing state, or the currently 
// grabbed primitive. Affects wireframe and things like that.
// Example:
// (line-width 5)
// (hint-wire)
// (build-sphere 10 10) ; make a sphere with thick wireframe
// EndFunctionDoc

Scheme_Object *line_width(int argc, Scheme_Object **argv)
{
 	ArgCheck("line-width", "f", argc, argv);
    Engine::Get()->State()->LineWidth=FloatFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// point-width value
// Description: 
// Sets the point width (in screen space) of the current drawing state, or the currently 
// grabbed primitive. Affects point rendering and particles in hardware point mode.
// Example:
// (point-width 5)
// (hint-points)
// (build-sphere 10 10) ; make a sphere with thick points
// EndFunctionDoc

Scheme_Object *point_width(int argc, Scheme_Object **argv)
{
  	ArgCheck("point-width", "f", argc, argv);
    Engine::Get()->State()->PointWidth=FloatFromScheme(argv[0]);
    return scheme_void;
}

// StartFunctionDoc
// blend-mode src dst
// Description: 
// Sets the blend mode of the current drawing state, or the currently grabbed primitive. 
// This is the way that alpha is composited to the rendering surface.
// Example:
// (point-width 5)
// (hint-points)
// (build-sphere 10 10) ; make a sphere with thick points
// EndFunctionDoc

Scheme_Object *blend_mode(int argc, Scheme_Object **argv)
{
  	ArgCheck("blend-mode", "ss", argc, argv);
	char *s=StringFromScheme(argv[0]);	
	char *d=StringFromScheme(argv[1]);	
    Engine::Get()->State()->SetBlendMode(s,d);
    return scheme_void;
}

// StartFunctionDoc
// hint-solid
// Description: 
// Sets the render hints to solid of the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-solid) ; this is the default render style so this isn't too exciting
// (build-cube) ; make a solid rendered cube 
// EndFunctionDoc

Scheme_Object *hint_solid(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_SOLID;
    return scheme_void;
}

// StartFunctionDoc
// hint-wire
// Description: 
// Sets the render hints to wireframe of the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-wire)
// (build-cube) ; make a wirefame rendered cube 
// EndFunctionDoc

Scheme_Object *hint_wire(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_WIRE;
    return scheme_void;
}

// StartFunctionDoc
// hint-normal
// Description: 
// Sets the render hints to display normals in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-normal)
// (build-cube) ; display the normals on this cube 
// EndFunctionDoc

Scheme_Object *hint_normal(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_NORMAL;
    return scheme_void;
}

// StartFunctionDoc
// hint-points
// Description: 
// Sets the render hints to display points in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-points)
// (build-cube) ; display the vertex points on this cube 
// EndFunctionDoc

Scheme_Object *hint_points(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_POINTS;
    return scheme_void;
}

// StartFunctionDoc
// hint-anti-alias
// Description: 
// Sets the render hints to anti-alias in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-anti-alias)
// (build-cube) ; display a smoothed cube 
// EndFunctionDoc

Scheme_Object *hint_anti_alias(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_AALIAS;
    return scheme_void;
}

// StartFunctionDoc
// hint-unlit
// Description: 
// Sets the render hints to unlit in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-unlit)
// (build-cube) ; display an unlit cube
// EndFunctionDoc

Scheme_Object *hint_unlit(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_UNLIT;
    return scheme_void;
}

// StartFunctionDoc
// hint-vertcols
// Description: 
// Sets the render hints to use vertex colours in the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. Vertex colours override the current (colour) state.
// Example:
// (hint-vertcols)
// (define mycube (build-cube)) ; make a cube with vertcols enabled
// (grab mycube)
// (pdata-set "c" 0 (vector 0 1 0)) ; set the colour of the first vertex to green
// (ungrab)
// EndFunctionDoc

Scheme_Object *hint_vertcols(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_VERTCOLS;
    return scheme_void;
}

// StartFunctionDoc
// hint-boc
// Description: 
// Sets the render hints to bounding box display in the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-box)
// (build-sphere 10 10) ; make a sphere with bounding box displayed
// EndFunctionDoc

Scheme_Object *hint_box(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_BOUND;
    return scheme_void;
}

// StartFunctionDoc
// hint-multitex
// Description: 
// Sets the render hints to use multitexturing in the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-multitexture)
// (multitexture 0 (load-texture "tex1.png"))
// (multitexture 1 (load-texture "tex2.png"))
// (build-sphere 10 10) ; make a sphere with overlayed textures
// EndFunctionDoc

Scheme_Object *hint_multitex(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_MULTITEX;
    return scheme_void;
}

// StartFunctionDoc
// hint-none
// Description: 
// Clears the render hints in the current drawing state, or the currently grabbed primitive.
// This allows you mainly to get rid of the default solid style, but also means that you can
// turn on and off hints without using push or pop.
// Example:
// (hint-none)
// (hint-wire)
// (build-cube) ; make a cube only visible with wireframe
// EndFunctionDoc

Scheme_Object *hint_none(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints=0;
    return scheme_void;
}

Scheme_Object *hint_origin(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_ORIGIN;
    return scheme_void;
}

Scheme_Object *hint_cast_shadow(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_CAST_SHADOW;
    return scheme_void;
}

Scheme_Object *hint_ignore_depth(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_IGNORE_DEPTH;	
    return scheme_void;
}

Scheme_Object *texture(int argc, Scheme_Object **argv)
{
  	ArgCheck("texture", "i", argc, argv);
	Engine::Get()->State()->Textures[0]=(int)IntFromScheme(argv[0]);
    return scheme_void;
}

Scheme_Object *multitexture(int argc, Scheme_Object **argv)
{
  	ArgCheck("multitexture", "ii", argc, argv);
	Engine::Get()->State()->Textures[IntFromScheme(argv[0])]=IntFromScheme(argv[1]);
    return scheme_void;
}

Scheme_Object *print_scene_graph(int argc, Scheme_Object **argv)
{
	Engine::Get()->Renderer()->PrintSceneGraph();
	return scheme_void;
}

Scheme_Object *hide(int argc, Scheme_Object **argv)
{
  	ArgCheck("hide", "i", argc, argv);
	if (Engine::Get()->Grabbed()) 
	{
		Engine::Get()->Grabbed()->Hide(FloatFromScheme(argv[0]));
	}
	return scheme_void;
}

Scheme_Object *selectable(int argc, Scheme_Object **argv)
{
  	ArgCheck("selectable", "i", argc, argv);
	if (Engine::Get()->Grabbed()) 
	{
		Engine::Get()->Grabbed()->Selectable(FloatFromScheme(argv[0]));
	}
	return scheme_void;
}

Scheme_Object *shader(int argc, Scheme_Object **argv)
{
  	ArgCheck("shader", "ss", argc, argv);
	
	char *vert=StringFromScheme(argv[0]);
	char *frag=StringFromScheme(argv[1]);
	
 	GLSLShader *shader = new GLSLShader(vert,frag);
	
    // remove the old one
	if (Engine::Get()->State()->Shader)
	{
		delete Engine::Get()->State()->Shader;
	}
		
	Engine::Get()->State()->Shader=shader;
	
	return scheme_void;
}

Scheme_Object *shader_set(int argc, Scheme_Object **argv)
{	
   	ArgCheck("shader-set!", "l", argc, argv);
	GLSLShader *shader=Engine::Get()->State()->Shader;

	if (shader)
	{
		// vectors seem easier to handle than lists with this api
		Scheme_Object *paramvec = scheme_list_to_vector(argv[0]);

		// apply to set parameters
		shader->Apply();

		Scheme_Object **vecptr = SCHEME_VEC_ELS(paramvec);

		for (int n=0; n<SCHEME_VEC_SIZE(paramvec); n+=2)
		{
			if (SCHEME_CHAR_STRINGP(vecptr[n]))
			{
				// get the parameter name
				char *param = StringFromScheme(vecptr[n]);

				// get the value
				Scheme_Object *arg=vecptr[n+1];

				if (SCHEME_NUMBERP(arg))
				{
					if (SCHEME_EXACT_INTEGERP(arg)) shader->SetInt(param,IntFromScheme(arg));
					else shader->SetFloat(param,(float)FloatFromScheme(arg));
				}
				else if (SCHEME_VECTORP(arg))
				{
					if (SCHEME_VEC_SIZE(arg) == 3)
					{
						dVector vec;
						FloatsFromScheme(arg,vec.arr(),3);
						shader->SetVector(param,vec);
					}
					else if (SCHEME_VEC_SIZE(arg) == 4)
					{
						dColour vec;
						FloatsFromScheme(arg,vec.arr(),4);
						shader->SetColour(param,vec);
					}
					else
					{	
						cerr<<"shader has found an argument vector of a strange size"<<endl;
					}
				}
				else
				{
					cerr<<"shader has found an argument type it can't send, numbers and vectors only"<<endl;
				}
			}
			else
			{
				cerr<<"shader has found a mal-formed parameter list"<<endl;
			}
		}
		GLSLShader::Unapply();
	}   
	
	return scheme_void;
}

void LocalStateFunctions::AddGlobals(Scheme_Env *env)
{	
	// renderstate operations
	scheme_add_global("push",scheme_make_prim_w_arity(push,"push",0,0), env);
	scheme_add_global("pop",scheme_make_prim_w_arity(pop,"pop",0,0), env);
	scheme_add_global("grab",scheme_make_prim_w_arity(grab,"grab",1,1), env);
    scheme_add_global("ungrab",scheme_make_prim_w_arity(ungrab,"ungrab",0,0), env);
    scheme_add_global("print-scene-graph",scheme_make_prim_w_arity(print_scene_graph,"print-scene-graph",0,0), env);
	scheme_add_global("apply-transform",scheme_make_prim_w_arity(apply,"apply",1,1), env);
	scheme_add_global("identity",scheme_make_prim_w_arity(flux_identity,"identity",0,0), env);
	scheme_add_global("concat",scheme_make_prim_w_arity(concat,"concat",1,1), env);
    scheme_add_global("translate",scheme_make_prim_w_arity(translate,"translate",1,1), env);
    scheme_add_global("rotate",scheme_make_prim_w_arity(rotate,"rotate",1,1), env);
    scheme_add_global("scale",scheme_make_prim_w_arity(scale,"scale",1,1), env);
	scheme_add_global("get-transform", scheme_make_prim_w_arity(get_transform, "get-transform", 0, 0), env);
    scheme_add_global("colour",scheme_make_prim_w_arity(colour,"colour",1,1), env);
    scheme_add_global("wire-colour",scheme_make_prim_w_arity(wire_colour,"wire-colour",1,1), env);
    scheme_add_global("opacity",scheme_make_prim_w_arity(opacity,"opacity",1,1), env);
    scheme_add_global("specular",scheme_make_prim_w_arity(specular,"specular",1,1), env);
    scheme_add_global("ambient",scheme_make_prim_w_arity(ambient,"ambient",1,1), env);
    scheme_add_global("emissive",scheme_make_prim_w_arity(emissive,"emissive",1,1), env);
	scheme_add_global("shinyness",scheme_make_prim_w_arity(shinyness,"shinyness",1,1), env);
	scheme_add_global("texture",scheme_make_prim_w_arity(texture,"texture",1,1), env);
	scheme_add_global("multitexture",scheme_make_prim_w_arity(multitexture,"multitexture",2,2), env);
    scheme_add_global("hint-solid",scheme_make_prim_w_arity(hint_solid,"hint-solid",0,0), env);
    scheme_add_global("hint-wire",scheme_make_prim_w_arity(hint_wire,"hint-wire",0,0), env);
    scheme_add_global("hint-normal",scheme_make_prim_w_arity(hint_normal,"hint-normal",0,0), env);
    scheme_add_global("hint-points",scheme_make_prim_w_arity(hint_points,"hint-points",0,0), env);
    scheme_add_global("hint-anti-alias",scheme_make_prim_w_arity(hint_anti_alias,"hint-anti-alias",0,0), env);
    scheme_add_global("hint-none",scheme_make_prim_w_arity(hint_none,"hint-none",0,0), env);
    scheme_add_global("hint-unlit",scheme_make_prim_w_arity(hint_unlit,"hint-unlit",0,0), env);
    scheme_add_global("hint-vertcols",scheme_make_prim_w_arity(hint_vertcols,"hint-vertcols",0,0), env);
    scheme_add_global("hint-box",scheme_make_prim_w_arity(hint_box,"hint-box",0,0), env);
    scheme_add_global("hint-multitex",scheme_make_prim_w_arity(hint_multitex,"hint-multitex",0,0), env);
    scheme_add_global("hint-origin",scheme_make_prim_w_arity(hint_origin,"hint-origin",0,0), env);
    scheme_add_global("hint-cast-shadow",scheme_make_prim_w_arity(hint_cast_shadow,"hint-cast-shadow",0,0), env);
    scheme_add_global("hint-ignore-depth",scheme_make_prim_w_arity(hint_ignore_depth,"hint-ignore-depth",0,0), env);
	scheme_add_global("line-width",scheme_make_prim_w_arity(line_width,"line-width",1,1), env);
	scheme_add_global("point-width",scheme_make_prim_w_arity(point_width,"point-width",1,1), env);
	scheme_add_global("blend-mode",scheme_make_prim_w_arity(blend_mode,"blend-mode",2,2), env);
    scheme_add_global("parent",scheme_make_prim_w_arity(parent,"parent",1,1), env);
	scheme_add_global("hide",scheme_make_prim_w_arity(hide,"hide",1,1), env);
	scheme_add_global("selectable",scheme_make_prim_w_arity(selectable,"selectable",1,1), env);
	scheme_add_global("shader",scheme_make_prim_w_arity(shader,"shader",2,2), env);
	scheme_add_global("shader-set!",scheme_make_prim_w_arity(shader_set,"shader-set!",1,1), env);
}
