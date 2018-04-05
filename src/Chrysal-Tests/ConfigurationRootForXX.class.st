Class {
	#name : #ConfigurationRootForXX,
	#superclass : #ChrysalConfiguration,
	#category : #'Chrysal-Tests'
}

{ #category : #'post hooks' }
ConfigurationRootForXX >> postTreat [

	self propertyAt: #newLevels put: self levels
]
