#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/time.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <curses.h> 

void readString(char *string, int maxSize);
unsigned int getTimestamp();
void sendPing();
void getIp(const char *ipString, struct in_addr *out);
void writeChar(unsigned char value, char *out);
void writeShort(unsigned short value, char *out);
void writeInt(unsigned int value, char *out);

void readString(char *string, int maxSize)
{
  int index = 0;
  while(index < maxSize)
  {
    string[index] = getch();
    printw ("%s", );
    index++;
  }
}

int main()
{
  initscr();
  noecho();

  //keypad ( stdscr, TRUE );

  printw("Welcome to desktop client v1!\n\n");

//  while(1)
//  {
//    switch (getch())
//    {
//      case 258:
//        printw ( "DOWN\n" );
//        break;
//      case 259:
//        printw ( "UP\n" );
//        break;
//      case 260:
//        printw ( "LEFT\n" );
//        break;
//      case 261:
//        printw ( "RIGHT\n" );
//        break;
//    }
//
//    refresh();
//  }

  char ipString[16];
  int port;

  bool hasAddress = false;
  while(!hasAddress)
  {
    // Read the IP the user wants to connect to
    printw("Game server IP\n");
    refresh();
    
    getstr(ipString);
    strtok(ipString, "\n");

    // Read the port
    printw("Game server Port\n");
    refresh();
    scanf("%d", &port);
    getchar();

    // Confirm
    printw("connect to %s:%d? (y/n)\n", ipString, port);
    char confirmation;
    
    confirmation = getchar();
    getchar();
    while(confirmation != 'y' && confirmation != 'n')
    {
      printw("eh?");
      confirmation = getchar();
      getchar();
    }

    hasAddress = confirmation == 'y';
  }

  printw("connecting to %s:%d...\n", ipString, port);
  
  // Create a socket
  int fd; // The file descriptor
  if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
  {
    printw("unable to create socket\n");
    return 0;
  }

  struct sockaddr_in myaddr;
  memset((char *)&myaddr, 0, sizeof(myaddr));
  myaddr.sin_family = AF_INET;
  myaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  myaddr.sin_port = htons(0);

  if (bind(fd, (struct sockaddr *)&myaddr, sizeof(myaddr)) < 0)
  {
    printw("unable to bind socket\n");
    close(fd);
    return 0;
  }

  unsigned int timestamp = getTimestamp();

  sendPing(fd, ipString, port);

  unsigned int ping = 0;

  printw("connected with ping: %d at %u\n", ping, timestamp);

  close(fd);
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
  unsigned char myIp[4];
  getIp(ip, &servaddr.sin_addr);

  char pingMessage[7];
  writeChar(1, pingMessage);
  writeShort(258, pingMessage + 1);
  writeShort(41, pingMessage + 3);

  sendto(socket, pingMessage, 7, 0, (struct sockaddr *)&servaddr, sizeof(servaddr));
}

//==============================================================================
// UTILS
//==============================================================================
void writeChar(unsigned char value, char *out)
{
  memcpy(out, &value, 1);
}

void writeShort(unsigned short value, char *out)
{
  unsigned short v = htons(value);
  memcpy(out, &v, 2);
}

void writeInt(unsigned int value, char *out)
{
  unsigned int v = htonl(value);
  memcpy(out, &value, 4);
}

void getIp(const char *ipString, struct in_addr *out)
{
  if(strcmp(ipString, "localhost") == 0)
  {
    inet_aton("127.0.0.1", out);
  }
  else
  {
    inet_aton(ipString, out);
  }
}