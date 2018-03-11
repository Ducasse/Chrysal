"
I generate a configuration class independent from Chrysal objects, but based on them. I'm like a macro expansionNow Chrysal configuration still need to be subclass of ChrysalConfiguration class. 

There are two ways of specify the configuration class: either using ==configurationClass:== or ==defineConfigurationClassNamed:packagedIn:==
	
The main public API is the method ==with:==


	
ConfigurationBuilder new 
	configurationClass: ConfigurationForPillar;
	with: ConfigurationDescriptionForPillar new
	
ConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForPillar packagedIn: 'Chrysal-ToBeMovedElsewhere'; 
	with: ConfigurationDescriptionForPillar new
	
	
	
	
ConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal'; 
	with: ConfigurationDescriptionForXX new
"
Class {
	#name : #ChrysalConfigurationBuilder,
	#superclass : #Object,
	#instVars : [
		'configurationClass'
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
ChrysalConfigurationBuilder >> addAccessorsAndConversionMethodsFrom: aDescription [
	aDescription items
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
ChrysalConfigurationBuilder >> addDeclaredItemsMethodFrom: aDescription [
	self configurationClass
		compile: (self declaredItems: aDescription)
		classified: 'declared items'
]

{ #category : #'add description' }
ChrysalConfigurationBuilder >> addDescription: description [
	self configurationClass class compile: (self descriptionMethodFrom: description) classified: 'description'
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
ChrysalConfigurationBuilder >> declaredItems: aDescription [

	
	^ String
		streamContents: [ :str | 
			str nextPutAll: 'declaredItems'.
			str cr; tab.
			str nextPutAll: '"generated code"'.
			str cr; tab.
			str nextPutAll: '^ #( '.
			aDescription items
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
ChrysalConfigurationBuilder >> descriptionMethodFrom: description [
		
	^ String streamContents: [ :s |
		s nextPutAll: 'description'; newLine.
		s nextPutAll: '^ ' 
		]
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
ChrysalConfigurationBuilder >> with: aDescription [
	"self new with: ConfigurationDescriptionForXX new"


	self addAccessorsAndConversionMethodsFrom: aDescription.
	self addDeclaredItemsMethodFrom: aDescription.
	self addUtilityMethods
	"such methods are necessary to convert items back and forth. These methods are generated to avoid to have configuration being a subclass of a configuration class"
]
