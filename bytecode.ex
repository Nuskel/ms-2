{ metadaten; u.a. offsets der blöcke }
{ protos für reflection }
{ global & static vars }
{ instructions }
{ cleanup }

-----

Offsets:

- protos
- globals
- instructions
- cleanup

Protos:

[name,fields:[superset][name:type:mods]]

Globals:

address: data (1B)

Instructions:
(Adressen anhand der Offsets)

INIT

!label0
mov int global[0], local[0]
add int local[0], local[0]
mov int local[0], global[0]
ret

call label0

STOP
CLEAN
EXIT
