// Top-level build file where you can add configuration options common to all sub-projects/modules.

buildscript {
    repositories {
        mavenLocal()
    }
}

plugins {
    alias(libs.plugins.androidApplication) apply false
    alias(libs.plugins.scalaAndroid) apply false
}
