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
	defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX
]]]


[[[
ChrysalConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal-Tests'; 
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

"
Class {
	#name : 'ChrysalConfigurationBuilder',
	#superclass : 'Object',
	#instVars : [
		'configurationClass',
		'items',
		'configurationRootClass'
	],
	#category : 'Chrysal-Model',
	#package : 'Chrysal',
	#tag : 'Model'
}

{ #category : 'add configuration accessors' }
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

{ #category : 'add configuration accessors' }
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
				classified: 'conversion'
			 ]
]

{ #category : 'add configuration accessors' }
ChrysalConfigurationBuilder >> addDeclaredItemsMethodFrom: descriptionItems [
	self configurationClass
		compile: (self declaredItems: descriptionItems)
		classified: 'declared items'
]

{ #category : 'add description' }
ChrysalConfigurationBuilder >> addDescription: descriptionItems [
	self configurationClass class 
		compile: (self descriptionMethodFrom: descriptionItems) 
		classified: 'description'
]

{ #category : 'add description' }
ChrysalConfigurationBuilder >> addDescriptionBodyIn: aStream forItems: descriptionItems [

	aStream print: (STON toStringPretty: descriptionItems)
]

{ #category : 'accessing' }
ChrysalConfigurationBuilder >> configurationClass [
	^ configurationClass
]

{ #category : 'public api' }
ChrysalConfigurationBuilder >> configurationClass: aClass [
	configurationClass := aClass
]

{ #category : 'accessing' }
ChrysalConfigurationBuilder >> configurationRootClass [
	^ configurationRootClass ifNil: [ configurationRootClass := self defaultChrysalConfigurationClass ]
]

{ #category : 'public api' }
ChrysalConfigurationBuilder >> configurationRootClass: aClass [
	"to define a different superclass than ChrysalConfiguration (a subclass of it to store specific behavior for example"
	 configurationRootClass := aClass
]

{ #category : 'add configuration accessors' }
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

{ #category : 'add configuration accessors' }
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

{ #category : 'add class definition' }
ChrysalConfigurationBuilder >> defaultChrysalConfigurationClass [
	^ ChrysalConfiguration 	
]

{ #category : 'add class definition' }
ChrysalConfigurationBuilder >> defineConfigurationClassNamed: aString packagedIn: packageName [
	
	(self configurationRootClass 
		<< aString asSymbol
		package: packageName) install.
		
	self configurationClass: (self class environment at: aString asSymbol).
	(self class environment at: aString asSymbol)	comment: 'I''m a generated class that manages the conversion of configuration items written in STON format to Pharo objects. I''m used for example in Pillar to manage ''xxx.conf'' file.  

I was generated by the ChrysalConfigurationBuilder based on a list of item descriptions (instances of subclasses of ChrysalItem).
To help you remember from which configuration items I was generated from, I have a class side method which when executed in presence of the generating package will return a list of items. Note that such items only exist at compile time. Therefore executing this method with only the runtime does not make any sense.

chrysalDescriptions
 ^ STON fromString: ''[
	StringConfigurationItem {
		#propertyName : #title,
		#default : ''my super cool book'',
		#noDefault : false
	}, ... ]''


Now to exist and be used I do not need the full Chrysal package, just the runtime containing the ChrysalConfiguration class. Note that I may inherit from one of its subclasses to support better customisation. 
		
		Chrysal is hosted at: github.com/Ducasse/Chrysal
		
		
	To generate me, a code snippet similar to the following one was executed: 
		
		ChrysalConfigurationBuilder new 
			configurationRootClass: ', self configurationRootClass name, ';
			defineConfigurationClassNamed: #', aString asSymbol, 
			' packagedIn: ', packageName, ';',
			' 
			withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX '.
			
]

{ #category : 'add description' }
ChrysalConfigurationBuilder >> descriptionMethodFrom: descriptionItems [
	"self new descriptionMethodFrom: ConfigurationDescriptionForXX itemDescriptionForXX "

	^ String
		streamContents: [ :s | 
			s nextPutAll: 'chrysalDescriptions'; cr.
			s tab; nextPutAll: '"I''m a generated method. I''m just here to help remembering how the class was generated. I should only be executed in presence of the configurationBuilder contained in the Chrysal package. Indeed the item objects only exist at compile-time and STON will recreate them."'.
			s cr.
			s tab; nextPutAll: ' ^ STON fromString: '.
			self addDescriptionBodyIn: s forItems: descriptionItems ]
]

{ #category : 'private' }
ChrysalConfigurationBuilder >> descriptionNamed: aName [

	^ items at: aName asSymbol
]

{ #category : 'public api' }
ChrysalConfigurationBuilder >> initializeDescription: descriptionItems [
	items := Dictionary new.
	descriptionItems
		do: [ :each | items at: each propertyName asSymbol put: each ]
]

{ #category : 'private' }
ChrysalConfigurationBuilder >> isPropertyDefined: aName [

	^ items keys includes: aName asSymbol
]

{ #category : 'add configuration accessors' }
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

{ #category : 'public api' }
ChrysalConfigurationBuilder >> withDescriptionItems: descriptionItems [
	"self new with: ConfigurationDescriptionForXX new items"

	self initializeDescription: descriptionItems.
	self addAccessorsAndConversionMethodsFrom: descriptionItems.
	self addDeclaredItemsMethodFrom: descriptionItems.
	self addDescription: descriptionItems
]
