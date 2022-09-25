# Chrysal


![https://github.com/Ducasse/Chrysal/workflows/currentStablePharo/badge.svg](https://github.com/Ducasse/Chrysal/workflows/currentStablePharo/badge.svg)
![https://github.com/Ducasse/Chrysal/workflows/matrix/badge.svg](https://github.com/Ducasse/Chrysal/workflows/matrix/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github//Ducasse/Chrysal/badge.svg?branch=master)](https://coveralls.io/github//Ducasse/Containers-Grid?branch=master)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)]()
<!-- [![Build status](https://ci.appveyor.com/api/projects/status/1wdnjvmlxfbml8qo?svg=true)](https://ci.appveyor.com/project/olekscode/dataframe)  -->



Chrysal is a library to express and manage external configurations expressed as JSON files. Chrysal is purely static, it means that given a list of items, a reader class is generated that manages the conversion between the two worlds (what the end-users is declaring and what the application needs). At run time a generated Chrysal configuration can be a subclass of a domain class, subclass of 
ChrysalConfiguration. This way we can define behavior that will not be lost during the recompilation of the Chrysal configuration (the one based on the actual description).

* Chrysal creates at compile-time a reader that is responsible for mapping end-user expressed configurations to their internal representations (for example a 'a/b/c.html' into a file reference object). 
* In addition at run time a configuration object can customize the default behavior of the generated reader. 
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
It supports composite and list of items too. Item descriptions are subclasses of `ChrysalItem`. 

For example the `NewLineConfigurationItem` manages how the word `#unix` will be converted into the correct platform specific encoding. 
This logic is defined in the class `NewLineConfigurationItem`.

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

The configuration builder will consume a configuration description as shown above and produce a configuration reader. 

Here is a typical way to invoke the builder. 

```
ChrysalConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX
```	

Note that it will generate a class and its associated comments so that we can regenerate it too.

## Example of Extensions

Pillar extends the `ChrysalConfiguration` (run time class) to be able to perform extra treatment.
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

## About Run time Dependencies

Note that neither the items (subclasses of `ChrysalItem` and `ChrysalItems`), nor the builder will be used at run time. 
There you can package your description outside of Chrysal. This is why you can also store a description configuration in a textual format. 

The only dependency needed at run time is the `Chrysal-Runtime`. This package is minimalistic and it only contains the class `ChrysalConfiguration` that will be extended by the generated configuration reader produced by the builder. 


## Adding New Configuration Items

Since a configuration item describes information that will be used to generate code, it acts as a static data (from that perspective it can be perceived as data to be fed to a macro expansion engine). 

JSON configurations consider the following as literals and not strings: number true false symbol string. Therefore the conversion is not needed. 
To extend the item hierarchy, a new class should defines the methods: `defaultDomainValueString` and `domainValueConversionString`.

```
defaultDomainValueString
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."
     ...
```

```
domainValueConversionString
	"Returns a string converting a string as written in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: here aValue is the name of the parameter of the generated method.
	
	For example for fileConfigurationItem (inputFile), 
	the result of the method will be used in the body of the following generated method 
	
	convertInputFile: aValue
	      ^ (FileSystem workingDirectory / aValue)
	
	 Parameter of the item like baseline should be accessed via self nameOfProperty"

	^ '^ aValue'

```

Read the class, `BooleanConfigurationItem` for a simple case and `NewLineConfigurationItem` for a bit more advanced case. 

## Loading

```
Metacello new
   baseline: 'Chrysal';
   repository: 'github://Ducasse/Chrysal';
   load.
```

## If you want to depend on it

```
spec 
   baseline: 'Chrysal' 
   with: [ spec repository: 'github://Ducasse/Chrysal' ].
```

## Known limits:
- Path management should be revisited. 

