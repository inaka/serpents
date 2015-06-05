#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/time.h>

unsigned int getTimestamp();
void sendPing();

int main()
{
  printf("Welcome to desktop client v1!\n\n");

  char ipString[16];
  int port;

  bool hasAddress = false;
  while(!hasAddress)
  {
    // Read the IP the user wants to connect to
    printf("Game server IP\n");
    
    fgets(ipString, sizeof(ipString), stdin);
    fflush(stdin);
    strtok(ipString, "\n");

    // Read the port
    printf("Game server Port\n");
    scanf("%d", &port);
    getchar();

    // Confirm
    printf("connect to %s:%d? (y/n)\n", ipString, port);
    char confirmation;
    
    confirmation = getchar();
    getchar();
    while(confirmation != 'y' && confirmation != 'n')
    {
      printf("eh?");
      confirmation = getchar();
      getchar();
    }

    hasAddress = confirmation == 'y';
  }

  printf("connecting to %s:%d...\n", ipString, port);
  
  // Create a socket
  int fd; // The file descriptor
  if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
  {
    printf("unable to create socket\n");
    return 0;
  }

  struct sockaddr_in myaddr;
  memset((char *)&myaddr, 0, sizeof(myaddr));
  myaddr.sin_family = AF_INET;
  myaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  myaddr.sin_port = htons(0);

  if (bind(fd, (struct sockaddr *)&myaddr, sizeof(myaddr)) < 0)
  {
    printf("unable to bind socket\n");
    return 0;
  }

  unsigned int timestamp = getTimestamp();

  sendPing(fd, ipString, port);

  unsigned int ping = 0;

  printf("connected with ping: %d at %u\n", ping, timestamp);

  return 0;
}

unsigned int getTimestamp()
{
  struct timeval tv;

  gettimeofday(&tv, NULL);
  return (unsigned int)(tv.tv_sec) * 1000 +
         (unsigned int)(tv.tv_usec) / 1000;
}

void sendPing(int socket, const char *ip, int port)
{
  struct sockaddr_in servaddr;
  memset((char*)&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(port);
  memcpy((void *)&servaddr.sin_addr, ip, strlen(ip));

  char *pingMessage[7];
  sendto(socket, pingMessage, 7, 0, (struct sockaddr *)&servaddr, sizeof(servaddr));
}