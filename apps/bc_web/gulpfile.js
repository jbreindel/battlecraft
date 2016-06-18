'use strict';

// dependencies
var gulp = require('gulp');
var sass = require('gulp-sass');
var bower = require('gulp-bower');

// directories
var STATIC_DIR = 'priv/static/';
var BOWER_DIR = STATIC_DIR + 'bower_components/';
var SASS_DIR = STATIC_DIR + 'scss/';
var CSS_DIR = STATIC_DIR + 'css/';
var FONT_DIR = STATIC_DIR + 'font/';

// lib directories
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
    		includePaths: [FONT_AWE_SASS_DIR]
    	}))
    	.pipe(gulp.dest(CSS_DIR));
});

//font files
gulp.task('font', ['bower'], function() {

	// copy font files
	return gulp.src(BOWER_DIR + 'font-awesome/fonts/*')
		.pipe(gulp.dest(FONT_DIR));
});

//default gulp task
gulp.task('default', ['css', 'font']);
