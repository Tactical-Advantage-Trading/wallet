
plugins {
    id("org.barfuin.gradle.taskinfo") version "2.2.0"
    alias(libs.plugins.androidApplication)
    alias(libs.plugins.scalaAndroid)
}

scala.scalaVersion = "2.11.12"

android {
    namespace = "trading.tacticaladvantage"
    compileSdk = 36

    defaultConfig {
        applicationId = "trading.tacticaladvantage"
        versionName = "1.0"
        versionCode = 1
        minSdk = 28
        targetSdk

        vectorDrawables {
            useSupportLibrary = true
        }
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            signingConfig = signingConfigs.getByName("debug")

            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }

    packaging {
        resources {
            excludes += "/META-INF/{AL2.0,LGPL2.1}"
        }
    }
}

dependencies {
    implementation(libs.androidx.core.ktx)
}