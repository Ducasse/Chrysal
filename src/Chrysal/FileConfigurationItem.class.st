Class {
	#name : #FileConfigurationItem,
	#superclass : #ConfigurationItem,
	#instVars : [
		'default'
	],
	#category : #Chrysal
}

{ #category : #accessing }
FileConfigurationItem >> default [

	^ default ifNil: [ default := 'bookDefaultName' ]
]

{ #category : #accessing }
FileConfigurationItem >> default: aString [ 
	default := aString
]

{ #category : #accessing }
FileConfigurationItem >> defaultDomainValueString [
	^ '(FileSystem workingDirectory / ''{1}'' asFileReference)' format: {self default}
]

{ #category : #accessing }
FileConfigurationItem >> possibleValuesString [
	"to be filled up with default value at compilation time."

	"'Today is {1}.' format: {Date today}."

	^ '(FileSystem workingDirectory / {1} asFileReference)'
]
