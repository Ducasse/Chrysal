"
I generate mappings based on items in the configurationClass specified. 

	
ConfigurationBuilder new 
	configurationClass: ConfigurationForPillar;
	with: ConfigurationDescriptionForPillar new
	
ConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForPillar packagedIn: 'PillarConfiguration'; 
	with: ConfigurationDescriptionForPillar new
"
Class {
	#name : #ConfigurationBuilder,
	#superclass : #Object,
	#instVars : [
		'configurationClass',
		'importerClass'
	],
	#category : #Chrysal
}

{ #category : #configuration }
ConfigurationBuilder >> accessorGenerationForItem: anItem [
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

{ #category : #configuration }
ConfigurationBuilder >> configurationClass [
	^ configurationClass
]

{ #category : #configuration }
ConfigurationBuilder >> configurationClass: aClass [
	configurationClass := aClass
]

{ #category : #configuration }
ConfigurationBuilder >> conversionGenerationForItem: anItem [
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

{ #category : #configuration }
ConfigurationBuilder >> defineConfigurationClassNamed: aString packagedIn: packageName [
	ChrysalConfiguration
		subclass: aString asSymbol
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: packageName.
	self configurationClass: (self class environment at: aString asSymbol)
]

{ #category : #configuration }
ConfigurationBuilder >> setterGenerationForItem: anItem [
	
	^ String
		streamContents: [ :str | 
			str nextPutAll: anItem propertyName, ': aProperty'.
			str cr; tab.
			str nextPutAll: '"generated code"'.
			str cr; tab.
			str nextPutAll:
					'^ self propertyAt: #' , anItem propertyName , ' put: aProperty' ]
]

{ #category : #configuration }
ConfigurationBuilder >> with: aDescription [
	"self new with: ConfigurationDescriptionForXX new"
	aDescription items do: [ :each | 
		self configurationClass 
			compile: (self accessorGenerationForItem: each) 	
			classified: 'accessing'. 
		self configurationClass  
			compile: (self setterGenerationForItem: each) 
			classified: 'accessing'.
		self configurationClass  
			compile: (self conversionGenerationForItem: each) 
			classified: 'conversion'].
]
