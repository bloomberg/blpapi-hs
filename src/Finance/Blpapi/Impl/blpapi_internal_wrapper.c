/* Copyright 2014. Bloomberg Finance L.P.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:  The above
 * copyright notice and this permission notice shall be included in all copies
 * or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */
#include "blpapi_internal_wrapper.h"
#include <stdlib.h>

void blpapi_Message_correlationId_improved(
        const blpapi_Message_t *message,
        blpapi_CorrelationId_t *cid,
        size_t index) {
    blpapi_CorrelationId_t oCid = blpapi_Message_correlationId(message, index);
    *cid = oCid;
}

void blpapi_CorrelationId_convertToWrapped(
        const blpapi_CorrelationId_t *cid,
        blpapi_CorrelationIdWrapped_t *wrappedCid) {
    wrappedCid->d_type = cid->valueType;
    wrappedCid->d_classId = cid->classId;
    wrappedCid->d_value = cid->value.intValue;
}

blpapi_CorrelationId_t *blpapi_CorrelationId_create() {
    return malloc(sizeof(blpapi_CorrelationId_t));
}

void blpapi_CorrelationId_destroy(blpapi_CorrelationId_t *cid) {
    free(cid);
}

void blpapi_CorrelationId_convertFromWrapped(
        const blpapi_CorrelationIdWrapped_t *wrappedCid,
        blpapi_CorrelationId_t *cid) {

    cid->valueType = wrappedCid->d_type;
    cid->classId = wrappedCid->d_classId;
    cid->value.intValue = wrappedCid->d_value;
}
