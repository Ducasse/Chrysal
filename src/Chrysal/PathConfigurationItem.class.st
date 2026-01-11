"
To describe path items.


(PathConfigurationItem new
	propertyName: 'outputDirectoryPath';
	default: 'build').
"
Class {
	#name : 'PathConfigurationItem',
	#superclass : 'ChrysalItem',
	#category : 'Chrysal-Model',
	#package : 'Chrysal',
	#tag : 'Model'
}

{ #category : 'accessing' }
PathConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration.
	
	Should generate somethin like:
	
		Path from: '.'
	"

	^ self pathCreationStringWithArgument: ('''{1}''' format: {self default})
]

{ #category : 'accessing' }
PathConfigurationItem >> defaultValue [
	"The default value is the working directory, represented as a dot"
	^ '.'
]

{ #category : 'accessing' }
PathConfigurationItem >> domainValueConversionString [
	"Returns a string converting a string as written in the configuration file to a pharo object. Note that this method is like the body of a macro that will be expanded in the configuration class: here aValue is the name of the parameter of the generated method.
	
	The result of the method will be used in the body of the following generated method (for the inputFile property)
	
	convertInputPath: aValue
	      ^ Path from: aValue
	
	 Parameter of the item like baseline should be accessed via self nameOfProperty"

	^ '^ ', (self pathCreationStringWithArgument: 'aValue')
]

{ #category : 'accessing' }
PathConfigurationItem >> pathCreationStringWithArgument: argument [

	^ 'Path from: {1}' format: { argument }
]
