#include <assert.h>
#include <plt/escheme.h>
#include "FluxusEngine.h"
#include "FluxusPrimitives.h"
#include "dada.h"
#include "GraphicsUtils.h"
#include "Common.h"
#include "LinePrimitive.h"
#include "TextPrimitive.h"
#include "ParticlePrimitive.h"
#include "LocatorPrimitive.h"
#include "PixelPrimitive.h"
#include "BlobbyPrimitive.h"

using namespace FluxusPrimitives;
using namespace fluxus;
using namespace Common;

Scheme_Object *build_cube(int argc, Scheme_Object **argv)
{
	PolyPrimitive *BoxPrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(BoxPrim);
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(BoxPrim));    
}


Scheme_Object *build_nurbs(int argc, Scheme_Object **argv)
{
	ArgCheck("build-nurbs", "i", argc, argv);
	NURBSPrimitive *Prim = new NURBSPrimitive();
	Prim->Resize(IntFromScheme(argv[0]));
	return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *build_polygons(int argc, Scheme_Object **argv)
{
	ArgCheck("build-polygons", "ii", argc, argv);
	PolyPrimitive *Prim = new PolyPrimitive((PolyPrimitive::Type)IntFromScheme(argv[0]));
	Prim->Resize(IntFromScheme(argv[1]));
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *build_sphere(int argc, Scheme_Object **argv)
{
	ArgCheck("build-sphere", "ii", argc, argv);
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(SphPrim));
}

Scheme_Object *build_plane(int argc, Scheme_Object **argv)
{
	ArgCheck("build-plane", "ii", argc, argv);
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
	if (argc==0)
	{
		MakePlane(PlanePrim);
   		return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(PlanePrim));
	}
	else if (argc==2)
	{
    	MakePlane(PlanePrim,IntFromScheme(argv[0]),IntFromScheme(argv[1]));
    	return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(PlanePrim));
	}
	
	cerr<<"build-plane takes 0 or 2 arguments"<<endl;
	return scheme_void;
}

Scheme_Object *build_cylinder(int argc, Scheme_Object **argv)
{
	ArgCheck("build-cylinder", "ii", argc, argv);
	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(CylPrim));
}

Scheme_Object *build_line(int argc, Scheme_Object **argv)
{
	ArgCheck("build-line", "i", argc, argv);
	LinePrimitive *Prim = new LinePrimitive();
	Prim->Resize(IntFromScheme(argv[0]));
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *build_text(int argc, Scheme_Object **argv)
{
	ArgCheck("build-text", "s", argc, argv);
	
	char *text=SCHEME_BYTE_STR_VAL(argv[0]);
	
	// 16*16 grid of letters
	TextPrimitive *TextPrim = new TextPrimitive(16/256.0f,16/256.0f,16,0);
	TextPrim->SetText(text,20,-20,0.018);
	
	return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(TextPrim));
}
	
Scheme_Object *build_nurbs_sphere(int argc, Scheme_Object **argv)
{
	ArgCheck("build-nurbs-sphere", "ii", argc, argv);
	NURBSPrimitive *SphPrim = new NURBSPrimitive;
    MakeNURBSSphere(SphPrim, 1, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(SphPrim));
}

Scheme_Object *build_nurbs_plane(int argc, Scheme_Object **argv)
{
	ArgCheck("build-nurbs-plane", "ii", argc, argv);
	NURBSPrimitive *Prim = new NURBSPrimitive;
    MakeNURBSPlane(Prim, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *build_particles(int argc, Scheme_Object **argv)
{
	ArgCheck("build-particles", "i", argc, argv);
	ParticlePrimitive *Prim = new ParticlePrimitive;
	int count=IntFromScheme(argv[0]);
	for (int i=0; i<count; i++)
	{
		Prim->AddParticle(dVector(0,0,0),dColour(0,0,0),dVector(0.1,0.1,0.1));
	}
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *build_locator(int argc, Scheme_Object **argv)
{
	LocatorPrimitive *Prim = new LocatorPrimitive();
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *build_pixels(int argc, Scheme_Object **argv)
{
	ArgCheck("build-pixels", "ii", argc, argv);
	PixelPrimitive *Prim = new PixelPrimitive(IntFromScheme(argv[0]), IntFromScheme(argv[1]));
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *upload_pixels(int argc, Scheme_Object **argv)
{	
	Primitive *Grabbed=FluxusEngine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->Upload();
		    return scheme_void;
		}
	}
	
	cerr<<"upload-pixels can only be called while a pixelprimitive is grabbed"<<endl;
    return scheme_void;
}

Scheme_Object *pixels2texture(int argc, Scheme_Object **argv)
{		
	ArgCheck("pixels->texture", "i", argc, argv);
	Primitive *Prim=FluxusEngine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (Prim) 
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Prim);
		if (pp)
		{
		    return scheme_make_integer_value(pp->GetTexture());
		}
	}
	
	cerr<<"pixels->texture can only be called on a pixelprimitive"<<endl;
    return scheme_void;
}

Scheme_Object *build_blobby(int argc, Scheme_Object **argv)
{
	ArgCheck("build-blobby", "ivv", argc, argv);
	
	dVector dim;
	FloatsFromScheme(argv[1],dim.arr(),3);
	dVector size;
	FloatsFromScheme(argv[2],size.arr(),3);
	BlobbyPrimitive *Prim = new BlobbyPrimitive((int)dim.x,(int)dim.y,(int)dim.z,size);
	int count=IntFromScheme(argv[0]);
	for (int i=0; i<count; i++)
	{
		Prim->AddInfluence(dVector(0,0,0),0);
	}
	
    return scheme_make_integer_value(FluxusEngine::Get()->Renderer()->AddPrimitive(Prim));
}

Scheme_Object *draw_instance(int argc, Scheme_Object **argv)
{    	
    FluxusEngine::Get()->Renderer()->RenderPrimitive(FluxusEngine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0])));
    return scheme_void;
}

Scheme_Object *draw_cube(int argc, Scheme_Object **argv)
{    	
    FluxusEngine::Get()->Renderer()->RenderPrimitive(FluxusEngine::StaticCube);
    return scheme_void;
}

Scheme_Object *draw_plane(int argc, Scheme_Object **argv)
{    	
    FluxusEngine::Get()->Renderer()->RenderPrimitive(FluxusEngine::StaticPlane);
    return scheme_void;
}

Scheme_Object *draw_sphere(int argc, Scheme_Object **argv)
{    	
    FluxusEngine::Get()->Renderer()->RenderPrimitive(FluxusEngine::StaticSphere);
    return scheme_void;
}

Scheme_Object *draw_cylinder(int argc, Scheme_Object **argv)
{    	
    FluxusEngine::Get()->Renderer()->RenderPrimitive(FluxusEngine::StaticCylinder);
    return scheme_void;
}

Scheme_Object *destroy(int argc, Scheme_Object **argv)
{
	ArgCheck("destroy", "i", argc, argv);
	int name=0;
	name=IntFromScheme(argv[0]);	
	
	Primitive *p=FluxusEngine::Get()->Renderer()->GetPrimitive(name);
	if (p)
	{
    	if (p->IsPhysicalHint())
    	{
    		//Fluxus->GetPhysics()->Free(name);
    	}
    	FluxusEngine::Get()->Renderer()->RemovePrimitive(name);
    }

    return scheme_void;
}

void FluxusPrimitives::AddGlobals(Scheme_Env *env)
{	
	scheme_add_global("draw-cube", scheme_make_prim_w_arity(draw_cube, "draw-cube", 0, 0), env);
	scheme_add_global("build-cube", scheme_make_prim_w_arity(build_cube, "build-cube", 0, 0), env);
	scheme_add_global("build-polygons", scheme_make_prim_w_arity(build_polygons, "build-polygons", 2, 2), env);
	scheme_add_global("build-nurbs", scheme_make_prim_w_arity(build_nurbs, "build-nurbs", 1, 1), env);
	scheme_add_global("build-sphere", scheme_make_prim_w_arity(build_sphere, "build-sphere", 2, 2), env);
	scheme_add_global("build-plane", scheme_make_prim_w_arity(build_plane, "build-plane", 0, 2), env);
	scheme_add_global("build-cylinder", scheme_make_prim_w_arity(build_cylinder, "build-cylinder", 2, 2), env);
	scheme_add_global("build-line", scheme_make_prim_w_arity(build_line, "build-line", 1, 1), env);
	scheme_add_global("build-text", scheme_make_prim_w_arity(build_text, "build-text", 1, 1), env);
	scheme_add_global("build-nurbs-sphere", scheme_make_prim_w_arity(build_nurbs_sphere, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-nurbs-plane", scheme_make_prim_w_arity(build_nurbs_plane, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-particles", scheme_make_prim_w_arity(build_particles, "build-particles", 1, 1), env);
	scheme_add_global("build-locator", scheme_make_prim_w_arity(build_locator, "build-locator", 0, 0), env);
	scheme_add_global("build-pixels", scheme_make_prim_w_arity(build_pixels, "build-pixels", 2, 2), env);
	scheme_add_global("pload-pixels", scheme_make_prim_w_arity(upload_pixels, "upload-pixels", 0, 0), env);
	scheme_add_global("pixels->texture", scheme_make_prim_w_arity(pixels2texture, "pixels->texture", 1, 1), env);
	scheme_add_global("build-blobby", scheme_make_prim_w_arity(build_blobby, "build-blobby", 3, 3), env);
	scheme_add_global("draw-instance", scheme_make_prim_w_arity(draw_instance, "draw-instance", 0, 0), env);
	scheme_add_global("draw-cube", scheme_make_prim_w_arity(draw_cube, "draw-cube", 0, 0), env);
	scheme_add_global("draw-plane", scheme_make_prim_w_arity(draw_plane, "draw-plane", 0, 0), env);
	scheme_add_global("draw-sphere", scheme_make_prim_w_arity(draw_sphere, "draw-sphere", 0, 0), env);
	scheme_add_global("draw-cylinder", scheme_make_prim_w_arity(draw_cylinder, "draw-cylinder", 0, 0), env);
	scheme_add_global("destroy", scheme_make_prim_w_arity(destroy, "destroy", 1, 1), env);
}
