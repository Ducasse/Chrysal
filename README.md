# Chrysal

Chrysal is a library to express and manage external configurations expressed as STON files. Chrysal is the successor of Cocoon but it does not use any magical things like DNU and Magritte. Chrysal is purely static, it means that given a list of items, a reader class is generated that manages the conversion between the two worlds (what the end-users is declaring and what the application needs). At runtime a generated Chrysal configuration can be a subclass of a domain subclass of 
Chrysal configuration. This way we can define behavior that will not be lost during the recompilation of the chrysal configuration (the one based on the actual description).

* Chrysal creates at compile time a reader that is responsible for mapping end-user expressed configuration to their internal representation (for example a 'a/b/c.html' into a a file reference object). 
* In addition at runtime a configuration objects can customize the default behavior of the generated reader. 
* Finally the configuration developer can extend Chrysal to support new data. 

## Example of configuration

Here is an example of a configuration. This configuration is the one of a pillar project. The end user specifies different values for given entry. 

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

For example the NewLineConfigurationItem manages how #unix will be converted into the correct platform specific encoding. 
This logic is defined in the class NewLineConfigurationItem.

```
ChrysalItem subclass: #NewLineConfigurationItem
	instanceVariableNames: 'defaultDomainObject defaultKey'
	classVariableNames: ''
	package: 'Chrysal-Model'
```


## Example of configuration description

The developer of a specific configuration will assemble together a list of configuration item. This is this list that will describe how the actual configuration will be managed. This list will be interpreted and eaten by a reader builder to generate a specific reader. 

Here is typical configuration description. 
```
itemDescriptionForXX
	"just a simple description of items for the tests"
	
	^ {(StringConfigurationItem new
		propertyName: #title;
		default: 'my super cool book';
		yourself).
	(NumberConfigurationItem new
		propertyName: #headingLevelOffset;
		default: 0;
		yourself).
	(BooleanConfigurationItem new
		propertyName: #verbose;
		beTrueAsDefault;
		yourself).
	(FolderConfigurationItem new
		propertyName: #outputDirectory;
		default: 'build').
	(FileConfigurationItem new
		propertyName: #mainDocument;
		default: 'book').
	(FileConfigurationItem new
		propertyName: #latexTemplate;
		default: '_support/templates/main.latex.mustache').
	(FileConfigurationItem new
		propertyName: #latexChapterTemplate;
		default: '_support/templates/chapter.latex.mustache').
	(FileConfigurationItem new
		propertyName: #htmlTemplate;
		default: '_support/templates/html.mustache').
	(FileConfigurationItem new
		propertyName: #htmlChapterTemplate;
		default: '_support/templates/html.mustache').
	(NewLineConfigurationItem new
		propertyName: #newLine;
		defaultIsUnix).
	(SymbolConfigurationItem new
		propertyName: #latexWriter;
		default: #latex:sbabook;
		yourself)	"may be we should turn it into a Pillar specific item that convert to a specific class".
	(CompositeConfigurationItem new
		propertyName: #printerComposite;
		default: #ChrysalCompositeDomain;
		fields: #(level renderAs capitalization numbering headerSize);
		yourself).
	(CompositeConfigurationItem new
		propertyName: #convertedComposite;
		default: #ChrysalConvertedCompositeDomain;
		fields: #(newLine htmlTemplate title);
		yourself).
	(ListConfigurationItem new
		propertyName: #levels;
		default: #OrderedCollection;
		element: 'printerComposite';
		yourself).
	(PathConfigurationItem new
		propertyName: #relativeSimple;
		default: 'simple';
		yourself).
	(PathConfigurationItem new
		propertyName: #relativeComplex;
		default: 'simple/simple1';
		yourself).
	(PathConfigurationItem new
		propertyName: #absoluteSimple;
		default: '/simple';
		yourself).	
	(PathConfigurationItem new
		propertyName: #absoluteComplex;
		default: '/simple/simple1';
		yourself)
	}
```

## Configuration Reader Builder

The configuration builder will consumes a configuration description as shown above and produces a configuration reader. 

Here is a typical way to invoke the builder. 
```
ChrysalConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX
```	

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
