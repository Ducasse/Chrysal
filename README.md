# Chrysal

Chrysal is a small library to offer the possibility to manage external configurations expressed as STON files. 
Chrysal supports the conversion of elements (file, new lines, custom domain specific,..) entities from a textual format to Pharo object. 
It supports composite and list of items too. 

Chrysal is the successor of Cocoon but it does not use any magical things like DNU and Magritte. Chrysal is purely static, it means that given a list of items, a class is generated that 
manages the conversion between the two worlds. At runtime a generated Chrysal configuration can be a subclass of a domain subclass of 
Chrysal configuration. This way we can define behavior that will not be lost during the recompilation of the chrysal configuration (the one 
based on the actual description)

Clients may define their own description items.

Known limits:
- I confused {} and [] in STON and I should fix it. 
