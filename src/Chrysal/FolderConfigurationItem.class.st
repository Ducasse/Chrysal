Class {
	#name : #FolderConfigurationItem,
	#superclass : #ConfigurationItem,
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
FolderConfigurationItem >> convertToDomainObject: aString [
	"right now we do not pay attention to have a baseDirectory property to parametrize the folderitem"

	^ FileSystem workingDirectory / aString
]

{ #category : #accessing }
FolderConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ '(FileSystem workingDirectory / ''{1}'')' format: {self default}
]

{ #category : #accessing }
FolderConfigurationItem >> defaultValue [
	"in the context of the configuration writer!"
	^ 'build'
]

{ #category : #unused }
FolderConfigurationItem >> possibleValuesString [
	"to be filled up with default value at compilation time."


	^ '(FileSystem workingDirectory / {1} asFileReference)'
]
