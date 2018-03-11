Class {
	#name : #FileConfigurationItem,
	#superclass : #Chrysal,
	#category : #'Chrysal-Model'
}

{ #category : #example }
FileConfigurationItem class >> example [
	<sampleInstance>
	
	^ FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book'
]

{ #category : #unused }
FileConfigurationItem >> convertToDomainObject: aString [
	"right now we do not pay attention to have a baseDirectory property to parametrize the folderitem"

	^ FileSystem workingDirectory / aString
]

{ #category : #accessing }
FileConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ '(FileSystem workingDirectory / ''{1}'')' format: {self default}
]

{ #category : #accessing }
FileConfigurationItem >> defaultValue [
	^ 'bookDefaultName'
]

{ #category : #accessing }
FileConfigurationItem >> domainValueConversionString [
	"Returns a string converting a string as written by in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: here aValue is the name of the parameter of the generated method.
	
	 Parameter of the item like baseline should be accessed via self nameOfProperty"

	^ '(FileSystem workingDirectory / aValue)' 
]

{ #category : #unused }
FileConfigurationItem >> possibleValuesString [
	"to be filled up with default value at compilation time."

	^ '(FileSystem workingDirectory / {1})'
]
