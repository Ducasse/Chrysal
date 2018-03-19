"
A chrysalItem is a description object responsible for generating statically a configuration class able to represent, convert, and manage default value of various configuration properties. 

A configuration is expressed as STON file on disk and the strings contained in the configuration should be converted to Pharo objects. For example files, folder, newline conventions. 

An item has basically a propertyName, an explanation string, a default value. 
Now care must be taken because item are compiled time objects and they should return string representations that will be used to generate the actual configuration class representing the configuration. 

For example NewLineConfigurationItem>> domainValueConversionStringis a good example. It returns a mapping that will be used to map configuration properties to Pharo objects.

NewLineConfigurationItem>> domainValueConversionString
	""Returns a string converting a string as written by in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: 
	here aValue is the name of the parameter of the generated method.
	""

	^ '(Dictionary
			newFrom:
				{(#cr -> String cr).
				(#lf -> UnixPlatform new lineEnding).
				(#crlf -> Win32Platform new lineEnding).
				(#unix -> UnixPlatform new lineEnding).
				(#mac -> UnixPlatform new lineEnding).
				(#dos -> Win32Platform new lineEnding)}) at: aValue'



Applications may extend or add new subclasses to manage specific property.


Known limitations:

Note that for now, I do not manage the fact that the user could define a default value when he does not specify a given value for a property. 
For example, the following configuration method

defaultScriptLanguage
	""generated code""
	^ self propertyAt: #defaultScriptLanguage ifAbsent: [(PRRealScriptLanguage for: 'smalltalk')]

is generated and we cannot simply let the user define that the default language could be something else than 'smalltalk'. Of course, he just has to specify a value for defaultScriptLanguage to do it - but we are talking about the default case specification. 






STON toStringPretty: ConfigurationDescriptionForXX new items

 '[
	StringConfigurationItem {
		#propertyName : ''title'',
		#default : ''my super cool book''
	},
	BooleanConfigurationItem {
		#propertyName : ''verbose'',
		#default : ''true''
	},
	StringConfigurationItem {
		#propertyName : ''attribution'',
		#default : ''me, myself and I''
	},
	StringConfigurationItem {
		#propertyName : ''series'',
		#default : ''Square Bracket Associate Collection''
	},
	StringConfigurationItem {
		#propertyName : ''keywords'',
		#default : ''Pharo''
	},
	FolderConfigurationItem {
		#propertyName : ''outputDirectory'',
		#default : ''build''
	},
	FileConfigurationItem {
		#propertyName : ''mainDocument'',
		#default : ''book''
	},
	FileConfigurationItem {
		#propertyName : ''latexTemplate'',
		#default : ''_support/templates/main.latex.mustache''
	},
	FileConfigurationItem {
		#propertyName : ''latexChapterTemplate'',
		#default : ''_support/templates/chapter.latex.mustache''
	},
	FileConfigurationItem {
		#propertyName : ''htmlTemplate'',
		#default : ''_support/templates/html.mustache''
	},
	FileConfigurationItem {
		#propertyName : ''htmlChapterTemplate'',
		#default : ''_support/templates/html.mustache''
	},
	NewLineConfigurationItem {
		#propertyName : ''newLine'',
		#defaultKey : #unix
	},
	SymbolConfigurationItem {
		#propertyName : ''latexWriter'',
		#default : #''latex:sbabook''
	}
]'
"
Class {
	#name : #ChrysalItem,
	#superclass : #Object,
	#instVars : [
		'propertyName',
		'default',
		'explanation'
	],
	#category : #'Chrysal-Model'
}

{ #category : #accessing }
ChrysalItem >> default [
	^ default ifNil: [ default := self defaultValue ]
]

{ #category : #accessing }
ChrysalItem >> default: aString [ 
	default := aString
]

{ #category : #accessing }
ChrysalItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self subclassResponsibility
]

{ #category : #accessing }
ChrysalItem >> defaultValue [
	"Default value is the default value of an item in the domain of the configuration writer and not in the Pharo world."
	^ 'defaultValue'
]

{ #category : #accessing }
ChrysalItem >> domainValueConversionString [
	"Returns a string converting a string as written in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: here aValue is the name of the parameter of the generated method.
	
	For example for fileConfigurationItem (inputFile), 
	the result of the method will be used in the body of the following generated method 
	
	convertInputFile: aValue
	      ^ (FileSystem workingDirectory / aValue)
	
	
	 Parameter of the item like baseline should be accessed via self nameOfProperty"

	^ 'aValue'
]

{ #category : #accessing }
ChrysalItem >> explanation [

	^ explanation
]

{ #category : #accessing }
ChrysalItem >> explanation: aString [

	explanation := aString
]

{ #category : #unused }
ChrysalItem >> possibleValuesString [
	^ ''
]

{ #category : #printing }
ChrysalItem >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: ' (' , propertyName printString;
		nextPutAll: ' ' , self default printString , ')'
]

{ #category : #accessing }
ChrysalItem >> propertyName [
	^ propertyName
]

{ #category : #accessing }
ChrysalItem >> propertyName: aString [
	"property name"

	propertyName := aString
]

{ #category : #printing }
ChrysalItem >> storeOn: aStream [

	
]
