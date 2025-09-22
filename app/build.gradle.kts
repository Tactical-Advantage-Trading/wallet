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
        versionName = "3.1"
        versionCode = 3
        targetSdk = 33
        minSdk = 28

        vectorDrawables {
            useSupportLibrary = true
        }

        externalNativeBuild {
            cmake {
                arguments("-DANDROID_STL=c++_shared")
            }
        }

        ndk {
            abiFilters.add("arm64-v8a")
            abiFilters.add("x86_64")
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

    externalNativeBuild {
        cmake {
            path = file("CMakeLists.txt")
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
    implementation(libs.zxing.android.embedded)
    implementation(libs.currencyedittext)
    implementation(libs.recyclerview)
    implementation(libs.appcompat)
    implementation(libs.multidex)
    implementation(libs.material)

    implementation(libs.secp256k1.kmp.jni.android)
    implementation(libs.scala.parser.combinators)
    implementation(libs.scodec.core)
    implementation(libs.akka.actor)
    implementation(libs.quicklens)
    implementation(libs.websocket)
    implementation(libs.rxscala)
    implementation(libs.web3j)

    implementation(libs.spray.json)
    implementation(libs.json4s.native)
    implementation(libs.commons.codec)
    implementation(libs.netty.all)
    implementation(libs.okhttp)
    implementation(libs.guava)
}