"
ChrysalConfigurationBuilder new
				configurationRootClass: ConfigurationRootForXX;
				defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal-Tests';
				withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX
"
Class {
	#name : #ConfigurationDescriptionForXX,
	#superclass : #Object,
	#category : #'Chrysal-Tests-Tests'
}

{ #category : #'for test and documentation' }
ConfigurationDescriptionForXX class >> itemDescriptionForXX [
	"just a simple description of items for the tests"
	
	^ {(StringConfigurationItem new
		propertyName: #title;
		default: 'my super cool book';
		yourself).
	(NumberConfigurationItem new
		propertyName: #headingLevelOffset;
		default: 0;
		yourself).
	(BooleanConfigurationItem new
		propertyName: #verbose;
		beTrueAsDefault;
		yourself).
	(StringConfigurationItem new
		propertyName: #attribution;
		default: 'me, myself and I';
		yourself).
	(StringConfigurationItem new
		propertyName: #series;
		default: 'Square Bracket Associate Collection';
		yourself).
	(StringConfigurationItem new
		propertyName: #keywords;
		default: 'Pharo';
		yourself).
	(FolderConfigurationItem new
		propertyName: #outputDirectory;
		default: 'build').
	(FileConfigurationItem new
		propertyName: #mainDocument;
		default: 'book').
	(FileConfigurationItem new
		propertyName: #latexTemplate;
		default: '_support/templates/main.latex.mustache').
	(FileConfigurationItem new
		propertyName: #latexChapterTemplate;
		default: '_support/templates/chapter.latex.mustache').
	(FileConfigurationItem new
		propertyName: #htmlTemplate;
		default: '_support/templates/html.mustache').
	(FileConfigurationItem new
		propertyName: #htmlChapterTemplate;
		default: '_support/templates/html.mustache').
	(NewLineConfigurationItem new
		propertyName: #newLine;
		defaultIsUnix).
	(SymbolConfigurationItem new
		propertyName: #latexWriter;
		default: #latex:sbabook;
		yourself)	"may be we should turn it into a Pillar specific item that convert to a specific class".
	(CompositeConfigurationItem new
		propertyName: #printerComposite;
		default: #ChrysalCompositeDomain;
		fields: #(level renderAs capitalization numbering headerSize);
		yourself).
	(CompositeConfigurationItem new
		propertyName: #convertedComposite;
		default: #ChrysalConvertedCompositeDomain;
		fields: #(newLine htmlTemplate title);
		yourself).
	(ListConfigurationItem new
		propertyName: #levels;
		default: #OrderedCollection;
		element: 'printerComposite';
		yourself).
	(PathConfigurationItem new
		propertyName: #relativeSimple;
		default: 'simple';
		yourself).
	(PathConfigurationItem new
		propertyName: #relativeComplex;
		default: 'simple/simple1';
		yourself).
	(PathConfigurationItem new
		propertyName: #absoluteSimple;
		default: '/simple';
		yourself).	
	(PathConfigurationItem new
		propertyName: #absoluteComplex;
		default: '/simple/simple1';
		yourself)
	.
	(StringConfigurationItem new
		propertyName: #titleWithoutDefault;
		default: 'my super cool book';
		noDefault; 
		yourself)	
	}
]
