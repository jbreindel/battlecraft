'use strict';

// dependencies
var gulp = require('gulp');
var sass = require('gulp-sass');
var bower = require('gulp-bower');
var soften = require('gulp-soften');
var elm = require('gulp-elm');

// directories
var STATIC_DIR = 'priv/static/';
var BOWER_DIR = STATIC_DIR + 'bower_components/';
var SASS_DIR = STATIC_DIR + 'scss/';
var CSS_DIR = STATIC_DIR + 'css/';
var FONT_DIR = STATIC_DIR + 'font/';
var ELM_DIR = STATIC_DIR + 'elm/';
var JS_DIR = STATIC_DIR + 'js/';

// lib directories
var BULMA_SASS_DIR = BOWER_DIR + 'bulma/';
var FONT_AWE_SASS_DIR = BOWER_DIR + 'font-awesome/scss/';

//bower install
gulp.task('bower', function() {

	// run bower
	return bower();
});

// compile sass files
gulp.task('css', ['bower'], function() {

	// run sass
    return gulp.src(SASS_DIR + '*.scss')
    	.pipe(sass({
    		includePaths: [BULMA_SASS_DIR, FONT_AWE_SASS_DIR]
    	}))
    	.pipe(gulp.dest(CSS_DIR));
});

// font files
gulp.task('font', ['bower'], function() {

	// copy font files
	return gulp.src(BOWER_DIR + 'font-awesome/fonts/*')
		.pipe(gulp.dest(FONT_DIR));
});

// initialize elm
gulp.task('elm.init', ['css', 'font'], function() {

	// change current working dir
	process.chdir(ELM_DIR);

	// initialize
	return elm.init();
});

// elm compiler
gulp.task('elm', ['css', 'font', 'elm.init'], function() {

	// bundle elm files
	return gulp.src('*.elm')
        .pipe(soften(2))
		.pipe(elm.bundle('game.js'))
		.pipe(gulp.dest('../js/'));
});

//default gulp task
gulp.task('default', ['css', 'font', 'elm']);
