#include <stdio.h>
#include <fcntl.h>
// #include <sys/stat.h>

#include <urweb/urweb.h>

/* Ur signature in Random.urs
val urandom: unit -> transaction (option int)
 */
uw_Basis_int * uw_Random_urandom(uw_context ctx, uw_unit u) {

        uw_Basis_int myRandomData[1];
        size_t bytesRead = 0;
        int gotError = 0 ;
        int fd ;
        if ((fd = open("/dev/urandom", O_RDONLY)) < 0) return NULL ;
        while (bytesRead < sizeof myRandomData && !gotError)
        {
                ssize_t bytes = read(fd, (void *) myRandomData + bytesRead,
                                          sizeof myRandomData - bytesRead);
                if (bytes < 0) gotError = 1 ;
                else bytesRead += bytes;
        }
        close(fd);
        if (gotError) return NULL ;

        uw_Basis_int *nr = uw_malloc(ctx, sizeof(uw_Basis_int));
        *nr = myRandomData[0];
        return nr;
}