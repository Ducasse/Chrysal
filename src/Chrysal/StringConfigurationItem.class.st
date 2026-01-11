"
To describe string items.
Literal in STON.

```
StringConfigurationItem new
		propertyName: 'title';
		default: 'my super cool book';
		yourself
```
"
Class {
	#name : 'StringConfigurationItem',
	#superclass : 'ChrysalItem',
	#category : 'Chrysal-Model',
	#package : 'Chrysal',
	#tag : 'Model'
}

{ #category : 'example' }
StringConfigurationItem class >> example [

	<sampleInstance>
	
	^ StringConfigurationItem new
		propertyName: #title;
		default: 'my super cool book';
		yourself.
]

{ #category : 'accessing' }
StringConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ '''', self default, ''''
]

{ #category : 'accessing' }
StringConfigurationItem >> defaultValue [
	"default value for the default value of the item expressed in the domain of the configuration writer"
	^ 'default value for ' , self propertyName
]

{ #category : 'conversion' }
StringConfigurationItem >> domainValueConversionString [
	"Returns a string converting a string as written by in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: 
	here aValue is the name of the parameter of the generated method.
	"
	
	^ '^ aValue'
]
