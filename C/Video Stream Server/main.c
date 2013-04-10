#include "rtspd.h"

// -----------------------------
// MAIN
int main(int argc, char* argv[]){
  
  if ( argc != 2 ){
    printf("Please provide port number\n");
    return -1;
  }
  
  int sockfd, new_fd;  // listen on sock_fd, new connection on new_fd
  struct addrinfo hints, *servinfo, *p;
  struct sockaddr_storage their_addr; // connector's address information
  socklen_t sin_size;
  int yes = 1;
  char s[INET6_ADDRSTRLEN];
  int rv;
  char * PORT = argv[1];
  srand ( time(NULL) );
  sem_init(&mutex, 0, 1);
  // thread stuff
  int client_fd = 0; // Socket returned by accept
  pthread_t thread; // Thread to be created
        
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE; // use my IP

  if ((rv = getaddrinfo(NULL, PORT, &hints, &servinfo)) != 0) {
      fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
      return 1;
  }

    // loop through all the results and bind to the first we can
  for(p = servinfo; p != NULL; p = p->ai_next) {
    if ((sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1) {
      perror("server: socket");
      continue;
    }
    if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1) {
      perror("setsockopt");
      exit(1);
    }
    if (bind(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
      close(sockfd);
      perror("server: bind");
      continue;
    }
    break;
  }

  if (p == NULL){
    fprintf(stderr, "server: failed to bind\n");
    return 2;
  }
  freeaddrinfo(servinfo); // all done with this structure

  if (listen(sockfd, BACKLOG) == -1) {
    perror("listen");
    exit(1);
  }

  printf("server: waiting for connections...\n");

  // Main loop to wait for connections
  while(1) {
    sin_size = sizeof their_addr;
    
    // Block here until new connections come in
    new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
    
    // new connection broke, go back up the while loop
    if (new_fd == -1) {
      perror("accept");
      continue;
    }
    
    // Print client info
    inet_ntop(their_addr.ss_family, get_in_addr((struct sockaddr *)&their_addr), s, sizeof s);
    printf("server: got connection from %s\n", s);
           
    // Got a connection
    // Spawn a thread for that client
    client_fd = new_fd; // Socket returned by accept
    // Creates the thread and calls the function serve_client.
    while( pthread_create(&thread, NULL, serve_client, (void *) (intptr_t) client_fd) != 0 );
    // Detaches the thread. This means that, once the thread finishes, it is destroyed.
    pthread_detach(thread);
  }

  return 0;
}