Class {
	#name : #FileConfigurationItem,
	#superclass : #ConfigurationItem,
	#category : #Chrysal
}

{ #category : #example }
FileConfigurationItem class >> example [
	<sampleInstance>
	
	^ FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book'
]

{ #category : #conversion }
FileConfigurationItem >> convertToDomainObject: aString [
	"right now we do not pay attention to have a baseDirectory property to parametrize the folderitem"

	^ FileSystem workingDirectory / aString
]

{ #category : #accessing }
FileConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ '(FileSystem workingDirectory / ''{1}'' asFileReference)' format: {self default}
]

{ #category : #accessing }
FileConfigurationItem >> defaultValue [
	^ 'bookDefaultName'
]

{ #category : #unused }
FileConfigurationItem >> possibleValuesString [
	"to be filled up with default value at compilation time."

	^ '(FileSystem workingDirectory / {1} asFileReference)'
]
