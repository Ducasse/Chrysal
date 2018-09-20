# Chrysal

Chrysal is a library to offer the possibility to manage external configurations expressed as STON files. 

* Chrysal creates at compile time a reader that is responsible for mapping end-user expressed configuration to their internal representation (for example a 'a/b/c.html' into a a file reference object). 
* In addition at runtime a configuration objects can customize the default behavior of the generated reader. 
* Finally the configuration developer can extend Chrysal to support new data. 

## Example of configuration

```
{
  "base_url": "/booklet-ReflectiveCore/html",
  "site_name": "Pharo Book",
  "title":"A simple reflective object kernel",
  "attribution":"Stéphane Ducasse",
  "series": "The Pharo TextBook Collection",
  "keywords": "project template, Pillar, Pharo, Smalltalk",
  "language": "en-UK",
  "epub-id": "urn:uuid:A1B0D67E-2E81-4DF5-9E67-A64CBE366809",
  "tocFile": "index.pillar",
  "latexWriter" : #'latex:sbabook',
  "newLine": #unix,
  "htmlWriter": #html
}
```
## Example of configuration element

Chrysal supports the conversion of elements (file, new lines, custom domain specific,..) entities from a textual format to Pharo object. 
It supports composite and list of items too. 


## Example of configuration description
Chrysal is the successor of Cocoon but it does not use any magical things like DNU and Magritte. Chrysal is purely static, it means that given a list of items, a reader class is generated that manages the conversion between the two worlds (what the end-users is declaring and what the application needs). At runtime a generated Chrysal configuration can be a subclass of a domain subclass of 
Chrysal configuration. This way we can define behavior that will not be lost during the recompilation of the chrysal configuration (the one 
based on the actual description).

## Example of extension

Pillar extends the ChrysalConfiguration (runtime class) to be able to perform extra treatment.
```
ChrysalConfiguration subclass: #ChrysalPillarishConfiguration
	instanceVariableNames: 'printer'
	classVariableNames: ''
	package: 'Pillar-Chrysal'
```

```
ChrysalPillarishConfiguration >> postTreat

	(self propertyAt: #levels ifAbsent: [^ self ]) do: [ 
			:levelSpec |
			| instance |
			instance := (self printer printerSpecFor: levelSpec renderAs).
			instance fillFromAnother: levelSpec. 
			self printer level: instance n: levelSpec level. 
			 ]
```

## Known limits:
- Path management should be revisited. 
- Chrysal should be better packaged. 
