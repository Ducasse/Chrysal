Class {
	#name : #FileConfigurationItem,
	#superclass : #ConfigurationItem,
	#instVars : [
		'default'
	],
	#category : #Chrysal
}

{ #category : #accessing }
FileConfigurationItem >> default: aString [ 
	default := aString
]

{ #category : #'as yet unclassified' }
FileConfigurationItem >> defaultDomainValueString [
	"to be filled up with default value at compilation time."

	"'Today is {1}.' format: {Date today}."

	^ '(FileSystem workingDirectory / {1} asFileReference)'
]

{ #category : #'as yet unclassified' }
FileConfigurationItem >> defaultValueString [
	"to be filled up with default value at compilation time."

	"'Today is {1}.' format: {Date today}."

	^ '(FileSystem workingDirectory / {1} asFileReference)'
]

{ #category : #accessing }
FileConfigurationItem >> possibleValuesString [
	"to be filled up with default value at compilation time."

	"'Today is {1}.' format: {Date today}."

	^ '(FileSystem workingDirectory / {1} asFileReference)'
]
