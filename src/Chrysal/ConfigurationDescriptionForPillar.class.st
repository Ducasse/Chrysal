"
I'm just holding some descriptions that are used to generate the configurationForPillar class.


ChrysalConfigurationBuilder new 
	defineConfigurationClassNamed: #ConfigurationForPillar 
	packagedIn: 'Chrysal-ToBeMovedElsewhere'; 
	withDescriptionItems: ConfigurationDescriptionForPillar itemDescriptionForPillar
"
Class {
	#name : #ConfigurationDescriptionForPillar,
	#superclass : #Object,
	#category : #'Chrysal-ToBeMovedElsewhere'
}

{ #category : #sample }
ConfigurationDescriptionForPillar class >> itemDescriptionForPillar [
	^ {(StringConfigurationItem new
		propertyName: 'title';
		default: 'my super cool book';
		yourself).
	(StringConfigurationItem new
		propertyName: 'attribution';
		default: 'me, myself and I';
		yourself).
	(NumberConfigurationItem new
		propertyName: 'headingLevelOffset';
		explanation:
			'Indicate how to convert from the level of a Pillar heading to the level of heading in your exported document.
	For example, a headingLevelOffset of 3 converts a 1st level Pillar heading to an <h4>in HTML';
		default: 0;
		yourself).
	(StringConfigurationItem new
		propertyName: 'series';
		default: 'Square Bracket Associate Collection';
		yourself).
	(StringConfigurationItem new
		propertyName: 'keywords';
		default: 'Pharo';
		yourself).
	(FolderConfigurationItem new
		propertyName: 'outputDirectory';
		default: 'build').
	(FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book').
	(FileConfigurationItem new
		propertyName: 'inputFile';
		default: 'book';
		explanation:
			'The Pillar file that must be transformed. You can also specify an input file at the end of the command-line interface. Previously there was no default value';
		default: 'book').
	(FileConfigurationItem new
		propertyName: 'latexTemplate';
		default: '_support/templates/main.latex.mustache').
	(BooleanConfigurationItem new
		propertyName: 'scrambledEmailAddresses';
		beFalseAsDefault).
	(BooleanConfigurationItem new
		propertyName: 'separateOutputFiles';
		explanation:
			'If true, each input file is exported to one output file. If false (the default), all input files are exported to outputFile.';
		beFalseAsDefault).
	(BooleanConfigurationItem new
		propertyName: 'renderStructureAsSlide';
		explanation:
			'When true (the default), Pillar will create a dedicated slide for each Pillar header. This parameter is meaningless when generating a written document';
		beFalseAsDefault).
	(FileConfigurationItem new
		propertyName: 'latexChapterTemplate';
		default: '_support/templates/chapter.latex.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlTemplate';
		default: '_support/templates/html.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlChapterTemplate';
		default: '_support/templates/html.mustache').
	(NewLineConfigurationItem new
		propertyName: 'newLine';
		defaultIsUnix).
	(StringConfigurationItem new
		propertyName: 'latexWriter';
		default: #latex:sbabook;
		yourself)	"may be we should turn it into a Pillar specific item that convert to a specific class"}
]
