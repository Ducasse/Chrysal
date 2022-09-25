"
I'm the superclass of the generated configuration classes. 
In essence, a configuration class will convert a JSON file into dictionary of a configuration objects (string, files, ....). 
A configuration object is a kind of dictionary with lookup (so that a configuration can have a parent).

I''m needed because I define some behavior for the creation of instances of such generated classes. I also make sure that conversion methods are invoked when creating instances of such subclasses. I''m packaged in the Chrysal-Runtime package because users of Chrysal may want to package their configuration without the full Chrysal builder. 

Here is a typical configuration file and its description.

### End user configuration files
Here is an example of a typical configuration file of Pillar

```json
{
  ""base_url"": """",
  ""site_name"": ""PBE9"",
  ""title"": ""Pharo 9 by Example"",
  ""attribution"": ""Stéphane Ducasse and Gordana Rakic with Sebastijan Kaplar and Quentin Ducasse"",
  ""series"": ""Square Bracket tutorials"",
  ""keywords"": ""project template, Pillar, Pharo, Smalltalk"",
  ""language"": ""en-UK"",
  #'""tocFile"": ""index.pillar"" if you have a index.pillar file containing the table of contents of your book': #comment,
  ""latexWriter"" : #'latex:sbabook',
  ""plugins"": [""PRCitationTransformer""],
  ""htmlWriter"": #html
}
```


### Pillar configuration description

To handle such kind of file the developer declared the following configuration _description_.

```
ConfigurationForPillar class >> chrysalDescriptions [
	""I''m a generated method. I''m just here to help remembering how the class was generated. I should only be executed in presence of the configurationBuilder contained in the Chrysal package. Indeed the item objects only exist at compile-time and STON will recreate them.""
	 ^ STON fromString: ''[
	PathConfigurationItem {
		#propertyName : #bibFile,
		#default : ''''book.bib'''',
		#explanation : ''''Identify the file to be taken as input for the bibliography. This is the .bib file used to resolve the ${cite:ref=Key}$ annotation.''''
	},
	StringConfigurationItem {
		#propertyName : #title,
		#default : ''''my super cool book''''
	},
	StringConfigurationItem {
		#propertyName : #attribution,
		#default : ''''me, myself and I''''
	},
	NumberConfigurationItem {
		#propertyName : #headingLevelOffset,
		#default : 0,
		#explanation : ''''Indicate how to convert from the level of a Pillar heading to the level of heading in your exported document.\r\tFor example, a headingLevelOffset of 3 converts a 1st level Pillar heading to an <h4>in HTML''''
	},
	StringConfigurationItem {
		#propertyName : #series,
		#default : ''''Square Bracket Associate Collection''''
	},
	StringConfigurationItem {
		#propertyName : #keywords,
		#default : ''''Pharo''''
	},
	PathConfigurationItem {
		#propertyName : #outputDirectory,
		#default : ''''build''''
	},
	FolderConfigurationItem {
		#propertyName : #baseDirectory,
		#default : ''''.''''
	},
	FileConfigurationItem {
		#propertyName : #outputFile,
		#default : ''''output'''',
		#explanation : ''''If separateOutputFiles is false, indicate the name of the output file. This can also be a write stream.''''
	},
	FileConfigurationItem {
		#propertyName : #mainDocument,
		#default : ''''book''''
	},
	FileConfigurationItem {
		#propertyName : #inputFile,
		#default : ''''book'''',
		#explanation : ''''The Pillar file that must be transformed. You can also specify an input file at the end of the command-line interface. Previously there was no default value''''
	},
	StringConfigurationItem {
		#propertyName : #latexTemplate,
		#default : ''''_support/templates/main.latex.mustache''''
	},
	BooleanConfigurationItem {
		#propertyName : #scrambledEmailAddresses,
		#default : ''''false''''
	},
	BooleanConfigurationItem {
		#propertyName : #separateOutputFiles,
		#default : ''''false'''',
		#explanation : ''''If true, each input file is exported to one output file. If false (the default), all input files are exported to outputFile.''''
	},
	BooleanConfigurationItem {
		#propertyName : #renderStructureAsSlide,
		#default : ''''true'''',
		#explanation : ''''When true (the default), Pillar will create a dedicated slide for each Pillar header. This parameter is meaningless when generating a written document''''
	},
	StringConfigurationItem {
		#propertyName : #latexChapterTemplate,
		#default : ''''_support/templates/chapter.latex.mustache''''
	},
	StringConfigurationItem {
		#propertyName : #htmlTemplate,
		#default : ''''_support/templates/html.mustache''''
	},
	StringConfigurationItem {
		#propertyName : #htmlChapterTemplate,
		#default : ''''_support/templates/html.mustache''''
	},
	NewLineConfigurationItem {
		#propertyName : #newLine,
		#default : #platform,
		#explanation : ''''The string that separates lines in the exported document. This is often either LF or CR\\+LF but any string is possible. Values are #(#cr #lf #crlf #unix #mac #dos #platform)'''',
		#defaultKey : #platform
	},
	StringConfigurationItem {
		#propertyName : #latexWriter,
		#default : #''''latex:sbabook''''
	},
	StringConfigurationItem {
		#propertyName : #htmlWriter,
		#default : #html
	},
	NumberConfigurationItem {
		#propertyName : #slideInTemplateForDeckJS,
		#default : 1,
		#explanation : ''''Number of slides in Template: Indicate the number of slides created by the DeckJS template. This is important to create anchors.''''
	},
	CodeBlockDefaultLanguageItem {
		#propertyName : #defaultScriptLanguage,
		#explanation : ''''Indicate the language in scripts when none is specified. This language is used for syntax highlighting. The currently supported languages are bash, css, html, http, json, javascript, pillar, sql, ston, shellcommands and smalltalk''''
	},
	CompositeConfigurationItem {
		#propertyName : #levelSpecification,
		#default : #PRTitlePrinterSpecification,
		#fields : [
			#level,
			#renderAs,
			#capitalization,
			#numbering,
			#headerSize
		]
	},
	ListConfigurationItem {
		#propertyName : #levels,
		#default : #OrderedCollection,
		#element : #levelSpecification
	},
	PathConfigurationItem {
		#propertyName : #tocFile,
		#default : ''''.'''',
		#explanation : ''''Identify the file to be taken as input for the root of the table of contents. When it is not specified, the table of contents will not be computed and displayed''''
	}
]''
]
```


"
Class {
	#name : #ChrysalConfiguration,
	#superclass : #CTEnvironment,
	#category : #'Chrysal-Runtime'
}

{ #category : #'instance creation others' }
ChrysalConfiguration class >> newFromConvertedDictionary: aDictionary [
	"Assuming that aDictionary contains values which are already objects (i.e., false and not 'false') create a configuration based on this dictionary"
	
	| conf |
	conf := self new.
	aDictionary associations do: [ :aBinding | conf propertyAt: aBinding key asSymbol  put: aBinding value ].
	^ conf
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromDictionary: stonDictionary [
	"It seems that pillar cocoon configuration semantics is to consider the argument as a ston dictionary, i.e., with strings and not objects as values."
	
	^ self newFromSTONDictionary: stonDictionary
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromDictionary: aSTONDictionary extendedWith: anotherDictionary [

	| configuration |
	configuration :=  self newFromSTONDictionary: aSTONDictionary.
	anotherDictionary associations do: [ :aBinding | configuration propertyAt: aBinding key put: aBinding value].
	^ configuration
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromFile: aFile [
	
	^ aFile readStreamDo: [ :input | self newFromStream: input ]
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromFile: aFile extendedWith: anotherDictionary [
	^ aFile readStreamDo: [ :input | self newFromStream: input extendedWith: anotherDictionary ]
]

{ #category : #'instance creation others' }
ChrysalConfiguration class >> newFromSTONDictionary: stonDictionary [
	"Create a configuration based on a dictionary with the objects as defined in STON. 'string' for files and not files. Now pay attention since some literal objects such as true, numbers are not represented as string in STON."
	
	| conf |
	conf := self new.
	stonDictionary associations do: [ :aBinding | conf setConvertedValueOf: aBinding ].
	conf postTreat.
	^ conf
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromStream: aStream [
	"Create a new configuration from the stream, values are converted accordingly to the properties.
	i.e., a string 'false' will be turned into a false object in the resulting configuration."
	^ self newFromDictionary: (STON fromStream: aStream)
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromStream: aStream extendedWith: anotherDictionary [
	^ self newFromDictionary: (STON fromStream: aStream) extendedWith: anotherDictionary 
			
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromString: aString [

	^ self newFromStream: aString readStream
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromString: aString extendedWith: anotherDictionary [
	^ self newFromStream: aString readStream extendedWith: anotherDictionary
]

{ #category : #conversion }
ChrysalConfiguration >> declaredItems [

	^ #()
]

{ #category : #'post hooks' }
ChrysalConfiguration >> postTreat [
	"I'm a hook so that my subclasses can perform some actions once a configuration is created."
]

{ #category : #conversion }
ChrysalConfiguration >> setConvertedValueOf: aBinding [
	"Set the converted value of aBinding value in the correct property of the receiver. aBinding should be property and a raw value as written in the configuration"

	(self declaredItems includes: aBinding key asSymbol)
		ifTrue: [ self setConvertedValueOfDeclaredProperty: aBinding ]
		ifFalse: [ self perform: #propertyAt:put: 
								with: aBinding key asSymbol 
								with: aBinding value ]
	
]

{ #category : #conversion }
ChrysalConfiguration >> setConvertedValueOfDeclaredProperty: aBinding [
	"Set the converted value of aBinding value in the correct property of the receiver. 
	aBinding should be property and a raw value as written in the configuration"

	| setter converted |
	setter := aBinding key capitalized , ':'.
	converted := self perform: (#convert , setter) asSymbol with: aBinding value.
	converted ifNil: [ self error: 'aBinding is not correctly spelt ' , aBinding ].
	self perform: setter uncapitalized asSymbol with: converted
]
