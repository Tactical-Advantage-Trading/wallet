#include <jni.h>
#include <string>
#include <cstdlib>
#include "node.h"

extern "C" jobject JNICALL
Java_trading_tacticaladvantage_Biconomy_startNodeWithArguments(JNIEnv *env, jobject, jobjectArray arguments) {
    jsize argument_count = env->GetArrayLength(arguments);
    int c_arguments_size = 0;

    for (int i = 0; i < argument_count ; i++) {
        auto elem = (jstring)env->GetObjectArrayElement(arguments, i);
        c_arguments_size += strlen(env->GetStringUTFChars(elem, nullptr));
        c_arguments_size++;
    }

    char* args_buffer = (char*)calloc(c_arguments_size, sizeof(char));
    char* current_args_position = args_buffer;
    char* argv[argument_count];

    for (int i = 0; i < argument_count ; i++) {
        auto elem = (jstring)env->GetObjectArrayElement(arguments, i);
        const char* current_argument = env->GetStringUTFChars(elem, nullptr);
        strncpy(current_args_position, current_argument, strlen(current_argument));
        argv[i] = current_args_position;

        // Increment to the next argument's expected position.
        current_args_position += strlen(current_args_position) + 1;
    }

    return jobject(node::Start(argument_count,argv));
}
