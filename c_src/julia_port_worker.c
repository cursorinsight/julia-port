#include <unistd.h>
#include <string.h>
#include <dlfcn.h>

#include <julia.h>
#include "ei.h"

#define INIT 1

static FILE* stderr_fp;

typedef struct julia_call_desc {
    char *module;
    char *function;
    char *data;
    long data_len;
} julia_call_desc_t;

void julia_call_desc_free(julia_call_desc_t* call_desc) {
    free(call_desc->module);
    free(call_desc->function);
    free(call_desc->data);
    free(call_desc);
}

void redirect_julia_stderr() {
    jl_function_t *func_fdio = jl_get_function(jl_base_module, "fdio");
    jl_function_t *func_redirect = jl_get_function(jl_base_module, "redirect_stderr");
    stderr_fp = tmpfile();
    int fd = fileno(stderr_fp);
    jl_value_t *arg_fd = jl_box_int64(fd);
    jl_value_t *arg_stream = jl_call1(func_fdio, arg_fd);
    jl_call1(func_redirect, arg_stream);
}

int init_julia(ei_x_buff* resBuf) {
    char* result_msg;
    const char *libjulia_path = getenv("JULIA_PORT_LIBJULIA_SO");
    void* handle = dlopen(libjulia_path, RTLD_GLOBAL | RTLD_LAZY);
    if (handle == NULL) {
        result_msg = "dl_open_failed";
        ei_x_encode_tuple_header(resBuf, 2);
        ei_x_encode_atom(resBuf, "error");
        ei_x_encode_binary(resBuf, result_msg, strlen(result_msg));
        return -1;
    }
    jl_init_with_image(NULL, libjulia_path);
    redirect_julia_stderr();
    ei_x_encode_atom(resBuf, "ok");

    return 0;
}

static char* get_exception_details()
{
    jl_function_t *func_bt = jl_get_function(jl_base_module, "catch_backtrace");
    jl_function_t *func_fdio = jl_get_function(jl_base_module, "fdio");
    jl_function_t *func_show = jl_get_function(jl_base_module, "display_error");
    jl_function_t *func_flush = jl_get_function(jl_base_module, "flush");


    jl_value_t *arg_ex = jl_exception_occurred();
    jl_value_t *arg_bt = jl_call0(func_bt);
    JL_GC_PUSH2(&arg_ex, &arg_bt);

    FILE* fp = tmpfile();
    int fd = fileno(fp);
    jl_value_t *arg_fd = jl_box_int64(fd);
    jl_value_t *arg_stream = jl_call1(func_fdio, arg_fd);
    jl_call3(func_show, arg_stream, arg_ex, arg_bt);
    jl_call1(func_flush, arg_stream);

    JL_GC_POP();

    int len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);
    char* buff = (char*)calloc(len + 1, sizeof(char));
    read(fd, buff, len);
    fclose(fp);
    return buff;
}

static int julia_port_call(julia_call_desc_t *call_desc,
                           ei_x_buff* resBuf)
{
    char* result_msg;
    // Get function pointer
    jl_module_t *module = (jl_module_t *)jl_eval_string(call_desc->module);
    if (module == NULL) {
        result_msg = "ModuleError";
        ei_x_encode_tuple_header(resBuf, 2);
        ei_x_encode_atom(resBuf, "error");
        ei_x_encode_binary(resBuf, result_msg, strlen(result_msg));
        return -1;
    }

    jl_function_t *func = jl_get_function(module, call_desc->function);
    if (func == NULL) {
        result_msg = "MethodError";
        ei_x_encode_tuple_header(resBuf, 2);
        ei_x_encode_atom(resBuf, "error");
        ei_x_encode_binary(resBuf, result_msg, strlen(result_msg));
        return -1;
    }

    // convert char* array argument to jl_array
    jl_value_t *array_type = jl_apply_array_type(jl_uint8_type, 1);
    jl_array_t *args_jl = jl_ptr_to_array_1d(array_type, call_desc->data, call_desc->data_len, 0);

    // function call
    jl_value_t *response = jl_call1(func, (jl_value_t *)args_jl);
    JL_GC_PUSH1(&response);

    // exception handling
    if (jl_exception_occurred()) {
        char* error_msg = get_exception_details();
        result_msg = error_msg;
        ei_x_encode_tuple_header(resBuf, 2);
        ei_x_encode_atom(resBuf, "error");
        ei_x_encode_binary(resBuf, result_msg, strlen(result_msg));
        free(error_msg);
        JL_GC_POP();
        return -1;
    } else {
        // convert julia result to char array
        ei_x_encode_tuple_header(resBuf, 2);
        ei_x_encode_atom(resBuf, "ok");
        ei_x_encode_binary(resBuf, (char*) jl_array_data(response), jl_array_len(response));

        JL_GC_POP();
        return 0;
    }
}

void parse_request(char* buff, int* buff_index, julia_call_desc_t *call_desc) {
    int size;
    long long_len;
    int type;
    int ver;

    ei_decode_version(buff, buff_index, &ver);

    ei_get_type(buff, buff_index, &type, &size);
    ei_decode_tuple_header(buff, buff_index, &size);

    // Reads module name from the buffer
    ei_get_type(buff, buff_index, &type, &size);
    call_desc->module = (char *)calloc(size + 1, sizeof(char));
    ei_decode_binary(buff, buff_index, call_desc->module, &long_len);

    // Reads function name from the buffer
    ei_get_type(buff, buff_index, &type, &size);
    call_desc->function = (char *)calloc(size + 1, sizeof(char));
    ei_decode_binary(buff, buff_index, call_desc->function, &long_len);

    // Reads arguments from the buffer
    ei_get_type(buff, buff_index, &type, &size);
    call_desc->data = (char *)calloc(size, sizeof(char));
    ei_decode_binary(buff, buff_index, call_desc->data, &long_len);
    call_desc->data_len = long_len;
}

int do_perform(char* buff, int* buff_index, ei_x_buff* resBuf) {
    int res;
    julia_call_desc_t *call_desc = (julia_call_desc_t*) malloc(sizeof(julia_call_desc_t));

    parse_request(buff, buff_index, call_desc);
    // Julia call
    res = julia_port_call(call_desc, resBuf);

    julia_call_desc_free(call_desc);

    return res;
}

void perform(int cmd, char* req, long* req_index, ei_x_buff* res) {
    switch (cmd) {
        case INIT: {
            init_julia(res);
            break;
        }
        default:
            do_perform(req, req_index, res);
            break;
    }
}
