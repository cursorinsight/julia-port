#include "gen_driver.h"

#include "ei.h"

/**
 * Callback to initialize the application-relevant state data when opening the
 * port driver and to return a pointer to the newly created driver state.
 */
void* init()
{
    static int driver_state_dummy = 1;
    return &driver_state_dummy;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to the driver state.
 */
void destroy(void *drv_state) {}

/**
 * Initialize any thread-specific data. This is called, when first dispatching
 * a request to a thread.
 */
void* thread_init()
{
    static int thread_state_dummy = 1;
    return &thread_state_dummy;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to thread-specific data.
 */
void thread_destroy(void *trd_state) {}

/**
 * Load balancing among threads. Balancing is implemented as a modulo
 * operation: % THREADS. Return null for round-robin strategy.
 */
unsigned int* balance(int cmd, unsigned char syn, unsigned int *key)
{
    *key = 1;
    return key;
}

void set_reply(gd_res_t* gd_res, char* result, long result_len) {
    if (gd_res->buf != NULL) {
        driver_free(gd_res->buf);
    }
    gd_res->buf = (char*) driver_alloc(result_len);
    memcpy(gd_res->buf, result, result_len);

    gd_res->len = result_len;
    gd_res->index = gd_res->len;
}

/**
 * Dispatch an asynchronous request by invoking the respective callback. If no
 * matching command is found, return an error.
 */
void dispatch(gd_req_t *req, gd_res_t *res, void *drv_state, void *trd_state)
{
    ei_x_buff resBuf;
    ei_x_new_with_version(&resBuf);

    perform(req->cmd, req->buf, &req->index, &resBuf);
    // Pretend that we've read the whole request
    req->index = req->len;

    // Copy the result to the appropriate container
    set_reply(res, resBuf.buff, resBuf.index);

    ei_x_free(&resBuf);
}
