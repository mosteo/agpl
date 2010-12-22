#include "agpl__interfaces__c.h"
#include "agpl__trace_c++.h"
#include <arpa/inet.h>
#include <errno.h>
#include <linux/if.h>
#include <linux/sockios.h>
#include <mcheck.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/vfs.h>
#include <unistd.h>

#define MAX_IFACES 16

using namespace agpl::trace;

const string log_section ("agpl.os_utils");

extern "C" {

  double agpl__os_utils__get_free_disk_space_c (char * path) {
    struct statfs info;

    if (statfs (path, &info) != 0) {
      perror ("agpl__os_utils__get_free_disk_space_c: ");
      return -1;
    }
    else
      return ((double) info.f_bsize) * (double)info.f_bfree;
  }

  int agpl__os_utils__spawn_c (char * command) {
    return system (command);
  }

  void agpl__os_utils__mtrace () {
    mtrace();
  }

  void agpl__os_utils__muntrace () {
    muntrace();
  }

  int agpl__os_utils__num_ifaces () {
    int sock;
    struct ifconf ifaces;
    char   buffer [sizeof (struct ifreq) * MAX_IFACES];

    sock = socket (AF_INET, SOCK_DGRAM, 0);

    if (sock <= 0) {
      perror ("num_ifaces.socket");
      close (sock);
      return 0;
    }

    ifaces.ifc_len = sizeof buffer;
    ifaces.ifc_buf = buffer;

    if (ioctl (sock, SIOCGIFCONF, &ifaces) < 0) {
      perror ("num_ifaces.ioctl");
      close (sock);
      return 0;
    }

    close (sock);
    return ifaces.ifc_len / sizeof (struct ifreq);
  }

  agpl_return_code agpl__os_utils__iface_addr (int index, char *addr) {
    /* Index is 1-based */
    /* addr must be a preallocated buffer */

    int sock;
    struct ifconf ifaces;
    struct ifreq  *iface;
    char   buffer [sizeof (struct ifreq) * MAX_IFACES];
    int num_ifaces = 0;

    sock = socket (AF_INET, SOCK_DGRAM, 0);

    if (sock <= 0) {
      perror ("iface_addr.socket");
      close (sock);
      *addr = 0;
      return agpl_return_err;
    }

    ifaces.ifc_len = sizeof buffer;
    ifaces.ifc_buf = buffer;

    if (ioctl (sock, SIOCGIFCONF, &ifaces)) {
      perror ("iface_addr.ioctl");
      close (sock);
      *addr = 0;
      return agpl_return_err;
    }

    close (sock);
    num_ifaces = ifaces.ifc_len / sizeof (struct ifreq);

    if (index < 1 || index > num_ifaces) {
      log ("os_utils.iface_addr: index out of range", ERROR, log_section);
      *addr = 0;
      return agpl_return_err;
    }

    iface = ifaces.ifc_ifcu.ifcu_req;

    strncpy
      (addr,
      inet_ntoa(((struct sockaddr_in *)&iface[index - 1].ifr_addr)->sin_addr),
      16);

    return agpl_return_ok;
  }

}
