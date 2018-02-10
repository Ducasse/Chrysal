Class {
	#name : #FolderConfigurationItem,
	#superclass : #ConfigurationItem,
	#instVars : [
		'default'
	],
	#category : #Chrysal
}

{ #category : #example }
FolderConfigurationItem class >> example [
	<sampleInstance>

	^ FolderConfigurationItem new
		propertyName: 'outputDirectory';
		default: 'build'
]

{ #category : #accessing }
FolderConfigurationItem >> default [

	^ default ifNil: [ default := 'build' ]
]

{ #category : #accessing }
FolderConfigurationItem >> default: aString [
	"normally this value should be the value of the instance for example 'build' "

	default := aString
]

{ #category : #unused }
FolderConfigurationItem >> defaultDomainValue [
	"'Today is {1}.' format: {Date today}."
	^ '(FileSystem workingDirectory / ''{1}'')' format: {self default}
]

{ #category : #accessing }
FolderConfigurationItem >> defaultDomainValueString [
	"'Today is {1}.' format: {Date today}."
	^ '(FileSystem workingDirectory / ''{1}'')' format: {self default}
]

{ #category : #unused }
FolderConfigurationItem >> defaultValueString [
	"to be filled up with default value at compilation time."
	"'Today is {1}.' format: {Date today}."
	^ (FileSystem workingDirectory / self default) fullName
]

{ #category : #unused }
FolderConfigurationItem >> possibleValuesString [
	"to be filled up with default value at compilation time."
	"'Today is {1}.' format: {Date today}."

	^ '(FileSystem workingDirectory / {1} asFileReference)'
]
