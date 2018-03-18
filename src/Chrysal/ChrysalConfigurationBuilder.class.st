"
I generate a configuration class independent from Chrysal objects, but based on them. I'm like a macro expansionNow Chrysal configuration still need to be subclass of ChrysalConfiguration class. 

There are two ways of specify the configuration class: either using ==configurationClass:== or ==defineConfigurationClassNamed:packagedIn:==
	
The main public API is the method ==withDescriptionItems:==

ChrysalConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForXX 
	packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ChrysalConfigurationBuilder itemDescriptionForXX
	
ChrysalConfigurationBuilder new 
	configurationClass: ConfigurationForXX;
	withDescriptionItems: ChrysalConfigurationBuilder itemDescriptionForXX
	

Next step is to 
- generate code that generate the class itself.  
- do we copy the methods from the superclass? this way we are fully automous?




"
Class {
	#name : #ChrysalConfigurationBuilder,
	#superclass : #Object,
	#instVars : [
		'configurationClass'
	],
	#category : #'Chrysal-Model'
}

{ #category : #'for test and documentation' }
ChrysalConfigurationBuilder class >> itemDescriptionForXX [
	"just a simple description of items for the tests"
	
	^ {(StringConfigurationItem new
		propertyName: 'title';
		default: 'my super cool book';
		yourself).
	(NumberConfigurationItem new
		propertyName: 'headingLevelOffset';
		default: 0;
		yourself).
	(BooleanConfigurationItem new
		propertyName: 'verbose';
		beTrueAsDefault;
		yourself).
	(StringConfigurationItem new
		propertyName: 'attribution';
		default: 'me, myself and I';
		yourself).
	(StringConfigurationItem new
		propertyName: 'series';
		default: 'Square Bracket Associate Collection';
		yourself).
	(StringConfigurationItem new
		propertyName: 'keywords';
		default: 'Pharo';
		yourself).
	(FolderConfigurationItem new
		propertyName: 'outputDirectory';
		default: 'build').
	(FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book').
	(FileConfigurationItem new
		propertyName: 'latexTemplate';
		default: '_support/templates/main.latex.mustache').
	(FileConfigurationItem new
		propertyName: 'latexChapterTemplate';
		default: '_support/templates/chapter.latex.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlTemplate';
		default: '_support/templates/html.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlChapterTemplate';
		default: '_support/templates/html.mustache').
	(NewLineConfigurationItem new
		propertyName: 'newLine';
		defaultIsUnix).
	(SymbolConfigurationItem new
		propertyName: 'latexWriter';
		default: #latex:sbabook;
		yourself)	"may be we should turn it into a Pillar specific item that convert to a specific class"}
]

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
				classified: 'conversion' ]
]

{ #category : #'add configuration accessors' }
ChrysalConfigurationBuilder >> addDeclaredItemsMethodFrom: descriptionItems [
	self configurationClass
		compile: (self declaredItems: descriptionItems)
		classified: 'declared items'
]

{ #category : #'add description' }
ChrysalConfigurationBuilder >> addDescription: descriptionItems [
	self configurationClass class compile: (self descriptionMethodFrom: descriptionItems) classified: 'description'
]

{ #category : #'add description' }
ChrysalConfigurationBuilder >> addDescriptionBodyIn: aStream forItems: descriptionItems [

	aStream print: (STON toStringPretty: descriptionItems)
]

{ #category : #accessing }
ChrysalConfigurationBuilder >> configurationClass [
	^ configurationClass
]

{ #category : #accessing }
ChrysalConfigurationBuilder >> configurationClass: aClass [
	configurationClass := aClass
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
			str nextPutAll: ('^ ', anItem domainValueConversionString ) ]
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
	self defaultChrysalConfigurationClass 
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

{ #category : #'public api' }
ChrysalConfigurationBuilder >> withDescriptionItems: descriptionItems [
	"self new with: ConfigurationDescriptionForXX new items"

	self addAccessorsAndConversionMethodsFrom: descriptionItems.
	self addDeclaredItemsMethodFrom: descriptionItems.
	self addDescription: descriptionItems
]
