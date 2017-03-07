# TpFMC
A model convertor for Transport Fever

TpFMC is a model convertor for Transport Fever.

The tools is based on the concept of TPMC (train-fever.com/forums/topic/train-fever-model-converter/), but rewritten in F#

So far it supports 3DS, OBJ, ASE and FBX2013

It's in general functionally equal to TPMC, but with some difference:

The meshes are not merged, instead, each mesh will result in a pair of output (msh, msh.blob)
The output filename is the mesh name
No XYZ coordinate swapping
No animation support for the moment

The advantage of this tool is that you can convert an ASE file at a time, and ASE meshes uses local coordinate in place of world coordinate, so no extra displacement parameters are needed.

Further improvement will be done, including:
* Multi-materials
* Object based export in place of mesh base export
* Animation support
* Perhaps a GUI
