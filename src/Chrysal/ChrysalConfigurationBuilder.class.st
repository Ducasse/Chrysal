"
I generate a configuration class independent from Chrysal objects (check itemDescriptionForXX for a sample of such description objects), but based on them. I'm like a macro expansion.

The generated configuration classes knows how to 
- access properties it stores
- convert values from the external world (in terms of STON) to Pharo objects.

Since we are using STON as an external format, the following objects are considered as literals and as such we do not 
convert them since they get converted by STON reader for us:  number, true, false, symbol and string.

The generated Chrysal configuration classes still needs to be subclass of ChrysalConfiguration class to reuse some of the behavior to create instances.

!! API
There are two ways of specify the configuration class: either using ==configurationClass:== or ==defineConfigurationClassNamed:packagedIn:==
	
The main public API is the method ==withDescriptionItems:==

Here are some typical usecases. 

[[[
ChrysalConfigurationBuilder new 
	configurationRootClass: ConfigurationRootForXX;
	defineConfigurationClassNamed: #ConfigurationForXX 
	packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX
]]]


[[[
ChrysalConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForXX 
	packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX
]]]

[[[	
ChrysalConfigurationBuilder new 
	configurationClass: ConfigurationForXX;
	withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX
]]]	


A ConfigurationItem can be an elementary element such as Number, String, Symbol, NewLine,
 File or Folder, but also a composite one. 

In case of composite, for now the composite are not converted using Chrysal but are plain STON values. 


!! Note for the chrysal extender
STON configurations consider the following as literals and not strings: number true false symbol string. Therefore the conversion is not needed. 
 
!! Potential todo

- Generate the invocation that generates the class itself, so that we can regenerate it. 




"
Class {
	#name : #ChrysalConfigurationBuilder,
	#superclass : #Object,
	#instVars : [
		'configurationClass',
		'items',
		'configurationRootClass'
	],
	#category : #'Chrysal-Model'
}

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> accessorGenerationForItem: anItem [
	"self new accessorGenerationForItem: BooleanConfigurationItem example"
	"self new accessorGenerationForItem: NewLineConfigurationItem example"
	"self new accessorGenerationForItem: FolderConfigurationItem example"
	"self new accessorGenerationForItem: FileConfigurationItem example"
	
	^ String
		streamContents: [ :str | 
			str nextPutAll: anItem propertyName.
			str cr; tab.
			str nextPutAll: '"generated code"'.
			str cr; tab.
			str nextPutAll:
					'^ self propertyAt: #' , anItem propertyName , ' ifAbsent: ['
						, anItem defaultDomainValueString , ']' ]
]

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> addAccessorsAndConversionMethodsFrom: descriptionItems [
	descriptionItems
		do: [ :each | 
			self configurationClass
				compile: (self accessorGenerationForItem: each)
				classified: 'accessing'.
			self configurationClass
				compile: (self setterGenerationForItem: each)
				classified: 'accessing'.
			self configurationClass
				compile: (self conversionGenerationForItem: each)
				classified: 'conversion'.
			self configurationClass 	
				compile: (self testingGenerationForItem: each)
				classified: 'testing'	
			 ]
]

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> addDeclaredItemsMethodFrom: descriptionItems [
	self configurationClass
		compile: (self declaredItems: descriptionItems)
		classified: 'declared items'
]

{ #category : #'add description' }
ChrysalConfigurationBuilder >> addDescription: descriptionItems [
	self configurationClass class 
		compile: (self descriptionMethodFrom: descriptionItems) 
		classified: 'description'
]

{ #category : #'add description' }
ChrysalConfigurationBuilder >> addDescriptionBodyIn: aStream forItems: descriptionItems [

	aStream print: (STON toStringPretty: descriptionItems)
]

{ #category : #accessing }
ChrysalConfigurationBuilder >> configurationClass [
	^ configurationClass
]

{ #category : #'public api' }
ChrysalConfigurationBuilder >> configurationClass: aClass [
	configurationClass := aClass
]

{ #category : #accessing }
ChrysalConfigurationBuilder >> configurationRootClass [
	^ configurationRootClass ifNil: [ configurationRootClass := self defaultChrysalConfigurationClass ]
]

{ #category : #'public api' }
ChrysalConfigurationBuilder >> configurationRootClass: aClass [
	"to define a different superclass than ChrysalConfiguration (a subclass of it to store specific behavior for example"
	 configurationRootClass := aClass
]

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> conversionGenerationForItem: anItem [
	"self new accessorGenerationForItem: FileConfigurationItem example"

	^ String
		streamContents: [ :str | 
			str nextPutAll: 'convert', (anItem propertyName) capitalized, ': aValue'.
			"pay attention aValue is a special argument that domainValueString can use to manipulate the parameter 
			during conversion"
			str cr; tab.
			str nextPutAll: '"generated code"'.
			str cr; tab.
			str nextPutAll: ( anItem domainValueConversionString ) ]
]

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> declaredItems: descriptionItems [

	
	^ String
		streamContents: [ :str | 
			str nextPutAll: 'declaredItems'.
			str cr; tab.
			str nextPutAll: '"generated code"'.
			str cr; tab.
			str nextPutAll: '^ #( '.
			descriptionItems
				do: [:each | str nextPutAll: '#', each propertyName]
				separatedBy: [str space].
			str nextPutAll: ' )' ]
]

{ #category : #'add class definition' }
ChrysalConfigurationBuilder >> defaultChrysalConfigurationClass [
	^ ChrysalConfiguration 	
]

{ #category : #'add class definition' }
ChrysalConfigurationBuilder >> defineConfigurationClassNamed: aString packagedIn: packageName [
	self configurationRootClass 
		subclass: aString asSymbol
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: packageName.
	self configurationClass: (self class environment at: aString asSymbol)
]

{ #category : #'add description' }
ChrysalConfigurationBuilder >> descriptionMethodFrom: descriptionItems [
	"self new descriptionMethodFrom: ChrysalConfigurationDescription itemDescriptionForXX "

	^ String
		streamContents: [ :s | 
			s nextPutAll: 'chrysalDescriptions'; cr.
			s nextPutAll: ' ^ STON fromString: '.
			self addDescriptionBodyIn: s forItems: descriptionItems ]
]

{ #category : #private }
ChrysalConfigurationBuilder >> descriptionNamed: aName [

	^ items at: aName asSymbol
]

{ #category : #'public api' }
ChrysalConfigurationBuilder >> initializeDescription: descriptionItems [
	items := Dictionary new.
	descriptionItems
		do: [ :each | items at: each propertyName asSymbol put: each ]
]

{ #category : #private }
ChrysalConfigurationBuilder >> isPropertyDefined: aName [

	^ items keys includes: aName asSymbol
]

{ #category : #sample }
ChrysalConfigurationBuilder >> itemDescriptionForPillar [
	^ {(StringConfigurationItem new
		propertyName: #title;
		default: 'my super cool book';
		yourself).
	(StringConfigurationItem new
		propertyName: #attribution;
		default: 'me, myself and I';
		yourself).
	(NumberConfigurationItem new
		propertyName: #headingLevelOffset;
		explanation:
			'Indicate how to convert from the level of a Pillar heading to the level of heading in your exported document.
	For example, a headingLevelOffset of 3 converts a 1st level Pillar heading to an <h4>in HTML';
		default: 0;
		yourself).
	(StringConfigurationItem new
		propertyName: #series;
		default: 'Square Bracket Associate Collection';
		yourself).
	(StringConfigurationItem new
		propertyName: #keywords;
		default: 'Pharo';
		yourself).
	(FolderConfigurationItem new
		propertyName: #outputDirectory;
		default: 'build').
	(FolderConfigurationItem new
		propertyName: #baseDirectory;
		default: '.').
	(FileConfigurationItem new
		propertyName: #outputFile;
		default: 'output';
		explanation:
			'If separateOutputFiles is false, indicate the name of the output file. This can also be a write stream.').
	(FileConfigurationItem new
		propertyName: #mainDocument;
		default: 'book').
	(FileConfigurationItem new
		propertyName: #inputFile;
		default: 'book';
		explanation:
			'The Pillar file that must be transformed. You can also specify an input file at the end of the command-line interface. Previously there was no default value').
	(StringConfigurationItem new
		propertyName: #latexTemplate;
		default: '_support/templates/main.latex.mustache').
	(BooleanConfigurationItem new
		propertyName: #scrambledEmailAddresses;
		beFalseAsDefault).
	(BooleanConfigurationItem new
		propertyName: #separateOutputFiles;
		explanation:
			'If true, each input file is exported to one output file. If false (the default), all input files are exported to outputFile.';
		beFalseAsDefault).
	(BooleanConfigurationItem new
		propertyName: #renderStructureAsSlide;
		explanation:
			'When true (the default), Pillar will create a dedicated slide for each Pillar header. This parameter is meaningless when generating a written document';
		beTrueAsDefault).
	(StringConfigurationItem new
		propertyName: #latexChapterTemplate;
		default: '_support/templates/chapter.latex.mustache').
	(StringConfigurationItem new
		propertyName: #htmlTemplate;
		default: '_support/templates/html.mustache').
	(StringConfigurationItem new
		propertyName: #htmlChapterTemplate;
		default: '_support/templates/html.mustache').
	(NewLineConfigurationItem new
		propertyName: #newLine;
		defaultIsUnix).
	(StringConfigurationItem new
		propertyName: #latexWriter;
		default: #latex:sbabook;
		yourself)	
		.
	(StringConfigurationItem new
		propertyName: #htmlWriter;
		default: #html;
		yourself)	
		.
	(NumberConfigurationItem new
		propertyName: #slideInTemplateForDeckJS;
		explanation: 'Number of slides in Template: Indicate the number of slides created by the DeckJS template. This is important to create anchors.';
		default: 1;
		yourself).
	(CodeBlockDefaultLanguageItem new)
	.
	(CompositeConfigurationItem new
		propertyName: #levelSpecification;
		default: #PRTitlePrinterSpecification;
		fields: #(level renderAs capitalization numbering headerSize);
		yourself)
	.
	(ListConfigurationItem new
		propertyName: #levels;
		default: #OrderedCollection;
		element: #levelSpecification;
		yourself) 
	.	
	(FileConfigurationItem new
		propertyName: #tocFile;
		noDefault)
	}
]

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> setterGenerationForItem: anItem [
	
	^ String
		streamContents: [ :str | 
			str nextPutAll: anItem propertyName, ': aValue'.
			str cr; tab.
			str nextPutAll: '"generated code"'.
			str cr; tab.
			str nextPutAll:
					'^ self propertyAt: #' , anItem propertyName , ' put: aValue' ]
]

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> testingGenerationForItem: anItem [
	"self new testingGenerationForItem: FileConfigurationItem example"

	^ String
		streamContents: [ :str | 
			str nextPutAll: anItem propertyName uncapitalized; nextPutAll: 'IfDefined: defBlockWithOneArg undefined: noDefBlock'.
			str cr; tab.
			str nextPutAll: '"generated code"'.
			str nextPutAll: 'When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time.'.
			str cr; tab.
			anItem hasDefault
				ifTrue: [ str nextPutAll: ' ^ defBlockWithOneArg value: self ' ; nextPutAll: anItem propertyName uncapitalized ] 
				ifFalse: [ str nextPutAll: ' ^ noDefBlock value']
			]
]

{ #category : #'public api' }
ChrysalConfigurationBuilder >> withDescriptionItems: descriptionItems [
	"self new with: ConfigurationDescriptionForXX new items"

	self initializeDescription: descriptionItems.
	self addAccessorsAndConversionMethodsFrom: descriptionItems.
	self addDeclaredItemsMethodFrom: descriptionItems.
	self addDescription: descriptionItems
]
