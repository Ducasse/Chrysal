"
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
	#name : #Chrysal,
	#superclass : #Object,
	#instVars : [
		'propertyName',
		'default'
	],
	#category : #'Chrysal-Model'
}

{ #category : #accessing }
Chrysal >> default [
	^ default ifNil: [ default := self defaultValue ]
]

{ #category : #accessing }
Chrysal >> default: aString [ 
	default := aString
]

{ #category : #accessing }
Chrysal >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self subclassResponsibility
]

{ #category : #accessing }
Chrysal >> defaultValue [
	"Default value is the default value of an item in the domain of the configuration writer and not in the Pharo world."
	^ 'defaultValue'
]

{ #category : #unused }
Chrysal >> possibleValuesString [
	^ ''
]

{ #category : #printing }
Chrysal >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: ' (' , propertyName printString;
		nextPutAll: ' ' , self default printString , ')'
]

{ #category : #accessing }
Chrysal >> propertyName [
	^ propertyName
]

{ #category : #accessing }
Chrysal >> propertyName: aString [
	"property name"

	propertyName := aString
]

{ #category : #printing }
Chrysal >> storeOn: aStream [

	
]
