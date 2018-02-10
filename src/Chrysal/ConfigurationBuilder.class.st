"
I generate mappings based on items in the configurationClass specified. 


ConfigurationBuilder new 
	configurationClass: ConfigurationForXX;
	with: ConfigurationDescriptionForXX new

ConfigurationBuilder new 
	configurationClass: ConfigurationForXX;
	setterGenerationForItem: BooleanConfigurationItem example
"
Class {
	#name : #ConfigurationBuilder,
	#superclass : #Object,
	#instVars : [
		'configurationClass'
	],
	#category : #Chrysal
}

{ #category : #'instance creation' }
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

{ #category : #'instance creation' }
ConfigurationBuilder >> configurationClass [
	^ configurationClass
]

{ #category : #'instance creation' }
ConfigurationBuilder >> configurationClass: aClass [
	configurationClass := aClass
]

{ #category : #'instance creation' }
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

{ #category : #'instance creation' }
ConfigurationBuilder >> with: aDescription [
	"self new with: ConfigurationDescriptionForXX new"
	aDescription items do: [ :each | 
		self configurationClass 
			compile: (self accessorGenerationForItem: each) 	
			classified: 'accessing'. 
		self configurationClass  
			compile: (self setterGenerationForItem: each) 
			classified: 'accessing'].
]
