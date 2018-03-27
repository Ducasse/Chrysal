"
I'm an item composed of other items. 

Here is an example how to define a CompositeConfigurationItem.

[[[
(CompositeConfigurationItem new
		propertyName: 'printerComposite';
		default: #ChrysalCompositeDomain;
		fields: #(level renderAs capitalization numbering headerSize);
		yourself)
]]]

It says that the composite will an instance of ChrysalCompositeDomain and that 
only these fields will be considered during the conversion. 

Right now, the value of such fields are not converted using Chrysal. We just get the STON elements. 


"
Class {
	#name : #CompositeConfigurationItem,
	#superclass : #ChrysalItem,
	#instVars : [
		'fields'
	],
	#category : #'Chrysal-Model'
}

{ #category : #example }
CompositeConfigurationItem class >> example [ 
	<sampleInstance>
	
	^ (CompositeConfigurationItem new
		propertyName: 'printerComposite';
		default: #ChrysalCompositeDomain;
		fields: #(level renderAs capitalization numbering headerSize);
		yourself)
]

{ #category : #accessing }
CompositeConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ ' {1} new ' format: {self default asString}
]

{ #category : #accessing }
CompositeConfigurationItem >> domainValueConversionString [
	"Returns a string converting a string as written in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: here aValue is the name of the parameter of the generated method.
	
	For example for fileConfigurationItem (inputFile), 
	the result of the method will be used in the body of the following generated method 
	
	convertInputFile: aValue
	      ^ (FileSystem workingDirectory / aValue)
	
	
	 Parameter of the item like baseline should be accessed via self nameOfProperty"
	^ '| composite |
	composite := {1} new.
	aValue do: [:v | 
			({2} includes: v key asSymbol)
				ifTrue: [ composite 
								perform:  (v key, '':'') asSymbol 
								with: v value].
		].
	^ composite' format: {self default asString . fields}
]

{ #category : #accessing }
CompositeConfigurationItem >> fields: aCollection [ 
	fields := aCollection
]

{ #category : #initialization }
CompositeConfigurationItem >> initialize [

	fields := #()
]
