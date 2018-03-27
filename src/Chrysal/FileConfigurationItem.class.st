"
To describe file items.

(FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book').
(FileConfigurationItem new
		propertyName: 'latexChapterTemplate';
		default: '_support/templates/chapter.latex.mustache').
"
Class {
	#name : #FileConfigurationItem,
	#superclass : #ChrysalItem,
	#category : #'Chrysal-Model'
}

{ #category : #example }
FileConfigurationItem class >> example [
	<sampleInstance>
	
	^ FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book'
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
	"Returns a string converting a string as written in the configuration file to a pharo object. Note that this method is like the body of a macro that will be expanded in the configuration class: here aValue is the name of the parameter of the generated method.
	
	The result of the method will be used in the body of the following generated method (for the inputFile property)
	
	convertInputFile: aValue
	      ^ (FileSystem workingDirectory / aValue)
	
	
	 Parameter of the item like baseline should be accessed via self nameOfProperty"

	^ '^ (FileSystem workingDirectory / aValue)' 
]

{ #category : #unused }
FileConfigurationItem >> possibleValuesString [
	"to be filled up with default value at compilation time."

	^ '(FileSystem workingDirectory / {1})'
]
