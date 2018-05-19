"
ChrysalConfigurationBuilder new 
	configurationRootClass: ConfigurationRootForXX;
	defineConfigurationClassNamed: #ConfigurationForXX 
	packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ChrysalConfigurationBuilder itemDescriptionForXX
"
Class {
	#name : #ConfigurationRootForXX,
	#superclass : #ChrysalConfiguration,
	#category : #'Chrysal-Tests-Tests'
}

{ #category : #'post hooks' }
ConfigurationRootForXX >> postTreat [

	self propertyAt: #newLevels put: self levels
]
