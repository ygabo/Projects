#include "rtspd.h"

// --------------------------------------
// SERVE CLIENT THREAD
// for delivering video to clients
void *serve_client(void *ptr){

  printf("Start of Thread\n");

  // Converts ptr back to integer.
  int new_fd = (int) (intptr_t) ptr;
  char buf [BUFFERLENGTH];
  char name[BUFFERLENGTH];
  char server_response[BUFFERLENGTH];
  char * frame_skip = 0;
  int ret = 0;
  int session = 10000000 + (rand() % 90000000); // 8 digits
  int cseq = 0;
  enum methods rtsp_method = 0;
  char * server_response_error = "RTSP/1.0 451 Invalid Parameter\n\n\0";
  
  // Timer stuff
  struct send_frame_data data;
  struct sigevent play_event;
  timer_t play_timer;
  struct itimerspec play_interval;
  
  // setup variables for the timer
  memset(&data, 0, sizeof(struct send_frame_data));
  data.socket_fd = new_fd;
  data.frames = 1;
  data.skip = 0;
  data.video = 0;
  data.is_opened = 0;
  data.state = INIT;
  data.cloud_server_name = 0;
  data.cloud_video_name = 0;
  data.cloud_port = 0;
  data.cloud_fd = 0;
  data.is_cloud = 0;  
  memset(data.last_ten_frames_size, -1, 10);  
  memset(&play_event, 0, sizeof(play_event));
  play_event.sigev_notify = SIGEV_THREAD;
  play_event.sigev_value.sival_ptr = &data;
  play_event.sigev_notify_function = send_frame;
  timer_create(CLOCK_REALTIME, &play_event, &play_timer);  
  data.play_timer = &play_timer;
  data.play_interval = &play_interval;
  
  // LOOP and receive messages
  while( (ret = recv(new_fd,buf,BUFFERLENGTH,0) ) > 0 ){

    // parse and get the method
    rtsp_method = getMethod( buf );

    if( rtsp_method != INVALID ){
      // get the cseq # first
      cseq = getCSeq( buf );
      switch( rtsp_method ){
        case SETUP:
          printf("SETUP\n");
          
          // parse the name from the buffer
          // store the video name in name
          getVideoName( buf, name );
                   
          // check if cloud
          data.is_cloud = isCloud( name );
          
          // if not cloud, try to open the video
          // send back error if:
          //  - a video is already open
          //  - not in INIT state
          //  - failed at opening video 
          if ( !data.is_cloud ) {
            if(!(data.video = cvCaptureFromFile(name)) 
                || data.is_opened || data.state != INIT ){
              // Error            
              sem_wait (&mutex);
              send(new_fd, server_response_error, strlen(server_response_error), 0); 
              sem_post (&mutex);
              break;
            }
          }
          else{
            data.cloud_video_name = name;
          }  
          
          // Success
          data.state = READY;
          data.is_opened = 1;
          sem_wait (&mutex);
          constructOKResponse( server_response, cseq, session );
          send(new_fd, server_response, strlen(server_response), 0);
          sem_post (&mutex);
          break;
        case PLAY:

          // If not in ready/play state, we dont do anything
          // Just send back an OK response as a way of ignoring
          // the user pressing the play button
          if( data.is_opened && data.state != INIT ) {
            printf("PLAY\n");
                       
            data.state = PLAYING;
            // look to see if you get a scale header
            frame_skip = strstr( buf , "Scale:" );
            // if there is, get the number and store it
            if( frame_skip ) data.skip = getFrameSkip( frame_skip );
                       
            // timer properties
            play_interval.it_interval.tv_sec = 0;
            play_interval.it_interval.tv_nsec = 40 * 1000000; // 40 for 40ms ms in ns
            play_interval.it_value.tv_sec = 0;
            play_interval.it_value.tv_nsec = 1; // can't be zero
              
            // this starts the timer 
            timer_settime(play_timer, 0, &play_interval, NULL);         
          }
          
          // send back an OK response
          sem_wait (&mutex);
          constructOKResponse( server_response, cseq, session );
          send(new_fd, server_response, strlen(server_response), 0);
          sem_post (&mutex);
          
          break;
        case PAUSE:
          printf("PAUSE\n");
          // only pause timer when PLAYING state
          if ( data.state == PLAYING ){          
            data.state = READY;
            play_interval.it_interval.tv_sec = 0;
            play_interval.it_interval.tv_nsec = 0;
            play_interval.it_value.tv_sec = 0;
            play_interval.it_value.tv_nsec = 0;
            timer_settime(play_timer, 0, &play_interval, NULL);
          }
          // send OK message
          // this will send even if not playing
          // (a way to ignore the button press if we're not playing anything)
          sem_wait (&mutex);
          constructOKResponse( server_response, cseq, session );
          send(new_fd, server_response, strlen(server_response), 0);
          sem_post (&mutex);
          break;
        case TEARDOWN:
          printf("TEARDOWN\n");
          // pause timer
          play_interval.it_interval.tv_sec = 0;
          play_interval.it_interval.tv_nsec = 0;
          play_interval.it_value.tv_sec = 0;
          play_interval.it_value.tv_nsec = 0;
          timer_settime(play_timer, 0, &play_interval, NULL);
          
          // reset the values
          data.state = INIT;
          data.is_opened = 0;
          data.frames = 1;
          memset(data.last_ten_frames_size, -1, 10);
          
          // send ok message
          sem_wait (&mutex);
          data.video = 0;
          constructOKResponse( server_response, cseq, session );
          send(new_fd, server_response, strlen(server_response), 0);
          sem_post (&mutex);
          break;
        default:
          printf("none - default\n");
          break;
      }
    }   
  }
  
  printf("End of Thread\n");
  timer_delete(play_timer);
  if (new_fd)
    close(new_fd);
  
  return 0;
}

// --------------------------------------
// HELPER
// This function will be called when the timer ticks
void send_frame(union sigval sv_data) {
  int total_length = 0, i = 0;
  int size_recv, total_size = 0, frame_size = -1;
  int sockfd,rv;
  struct addrinfo hints, *servinfo, *p;
  char port_num[1024];
  // openCV stuff
  IplImage *image, *image2;
  CvMat *encoded;
  // cloud stuff
  const struct cloud_server *cloud; 
  char chunk[BUFFERLENGTH];
  char *from_cloud = 0;
  char cloud_query[BUFFERLENGTH];
  // packet stuff
  struct RTP_packet *packet;
  struct send_frame_data *data = (struct send_frame_data *) sv_data.sival_ptr;
   
  // update frame number
  data->frames += data->skip;
  data->timestamp += 40;
  i = data->skip - 1;
  
  if (data->state == INIT ){
    // we are here if this function got called
    // after a teardown, this means turn off timer
    // and do nothing
    // redundant code but here for robustness
    data->play_interval->it_interval.tv_sec = 0;
    data->play_interval->it_interval.tv_nsec = 0;
    data->play_interval->it_value.tv_sec = 0;
    data->play_interval->it_value.tv_nsec = 0;
    timer_settime(*data->play_timer, 0, data->play_interval, NULL);
    
    memset(data->last_ten_frames_size, -1, 10);
    data->frames = 1;
    data->is_opened = 0;
    data->video = 0;
    return;
  }
  
  if( !data->is_cloud ){
    if(!data->video) return;
    image2 = cvQueryFrame( data->video );
    if (!image2){
      // end of video
      data->play_interval->it_interval.tv_sec = 0;
      data->play_interval->it_interval.tv_nsec = 0;
      data->play_interval->it_value.tv_sec = 0;
      data->play_interval->it_value.tv_nsec = 0;
      timer_settime(*data->play_timer, 0, data->play_interval, NULL);
      memset(data->last_ten_frames_size, -1, 10);
      
      data->video = 0;
      data->is_opened = 0;
      data->frames = 1;
      data->state = INIT;
      return;
    }
    // this is from 
    // http://stackoverflow.com/questions/4567188/read-successive-frames-opencv-using-cvqueryframe
    // used to skip frames
    image = cvCloneImage( image2 );
    while( i-- ){
      if( cvGrabFrame( data->video ) ){ 
        cvCopy( image2, image, 0 );
        image2 = cvRetrieveFrame( data->video, 1 );
        image = cvCloneImage( image2 ); 
        if (!((image2) != NULL && ((const IplImage*)(image2))->nSize == sizeof(IplImage))) return;
      }
    } 
    
    // Encode the frame in JPEG format with JPEG quality 30%.
    const static int encodeParams[] = { CV_IMWRITE_JPEG_QUALITY, 30 };
    encoded = cvEncodeImage(".jpeg", image, encodeParams);
    frame_size = encoded->cols;
  }
  else { // CLOUD
    cloud = get_cloud_server(data->cloud_video_name, data->frames);
    if( !cloud ) return ;
    
    if( compare_name(cloud->server, data->cloud_server_name ) ) { // not same
      // not same server so
      // close old socket file descriptor
      if (data->cloud_fd)
        close(data->cloud_fd);

      memset(&hints, 0, sizeof hints);
      hints.ai_family = AF_UNSPEC;
      hints.ai_socktype = SOCK_STREAM;
      sprintf(port_num, "%d", cloud->port);
      
      if ((rv = getaddrinfo(cloud->server, port_num, &hints, &servinfo)) != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
        return;
      }
      
      // loop through all the results and connect to the first we can
      for(p = servinfo; p != NULL; p = p->ai_next) {
          if ((sockfd = socket(p->ai_family, p->ai_socktype,
                  p->ai_protocol)) == -1) {
              perror("Error: can't connect to cloude socket");
              continue;
          }

          if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
              close(sockfd);
              perror("Error: can't connect to cloude socket");
              continue;
          }
          break;
      }

      if (p == NULL) {
          fprintf(stderr, "Failed to connect to cloud\n");
          return;
      }

      freeaddrinfo(servinfo); // all done with this structure
      
      // update data information
      data->cloud_server_name = cloud->server;
      data->cloud_port = cloud->port;
      data->cloud_fd = sockfd;
    }
    
    combineNameAndFrame( cloud_query, data->cloud_video_name, data->frames );
        
    // send name:frame to cloud server
    if ((send(data->cloud_fd, cloud_query, strlen( cloud_query ), 0)) == -1) {
	    perror("recv");
	    exit(1);
    }
   
    while( 1 ){    
      memset(chunk ,0 , BUFFERLENGTH);  //clear the variable
      if((size_recv = recv( data->cloud_fd , chunk , BUFFERLENGTH , 0) ) < 0){    
          break;
      }
      else{       
          if ( frame_size < 0 ) {
            // assuming we get more than 5 bytes on the first trip
            frame_size = get_frame_size( chunk );
            from_cloud = malloc( sizeof(char) * (frame_size+5) ); //with size header
          }
            
          memcpy(from_cloud+total_size, chunk, size_recv);
          total_size += size_recv;
          if( total_size >= frame_size+5) break;  
        }
    }
  }
  
  // fill up the packet with data 
  total_length = sizeof(struct RTP_packet) + ( frame_size * sizeof(char));
  packet = malloc( total_length );
  memset( packet, 0, total_length );
  packet->prefix = htons(0x2400);
  packet->length = htons(12 + frame_size);
  packet->header.first_two = htons(32794); // 1 on the farthest bit, 26 on first 8
  packet->header.sequence = htonl(data->frames);
  packet->header.timestamp = htonl(data->timestamp);
  
  if( data->is_cloud ){
    memcpy(packet->payload, from_cloud+5, frame_size);
  }
  else{
    memcpy(packet->payload, encoded->data.ptr, frame_size);
  }
  
  int total = 0;
  data->last_ten_frames_size[data->frames % 10] = frame_size;
  for( i = 0; i < 10; i ++ ) total += data->last_ten_frames_size[i];
  
  if ( data->state == PLAYING && total != 0){
    sem_wait (&mutex);
    send(data->socket_fd, packet, total_length, 0);
    sem_post (&mutex);
  }
  else{
    // We're here because we either ran out of stuff to send to client
    // or we're not in PLAYING state anymore, so stop timer
    data->play_interval->it_interval.tv_sec = 0;
    data->play_interval->it_interval.tv_nsec = 0;
    data->play_interval->it_value.tv_sec = 0;
    data->play_interval->it_value.tv_nsec = 0;
    timer_settime(*data->play_timer, 0, data->play_interval, NULL);
    
    if ( total == 0 ) data->state = INIT;
    
    // if not in READY state (and not PLAYING state), which means
    // we're in INIT state, reset some data
    if( data->state != READY ){
      memset(data->last_ten_frames_size, -1, 10);
      data->is_opened = 0; 
      data->frames = 1;
      data->video = 0;      
    }
  }
  
  if( packet )
    free( packet );
  if( from_cloud )
    free( from_cloud );
}

// --------------------------------------
// HELPER
// compare the names, not 0 if different
int compare_name( char* one, char * two ) {
  if ( !one || !two ) return 1;
  return strcmp( one, two);
}

// --------------------------------------
// HELPER
// gets the first 5 characters of a given buffer
// turns that into an integer and returns it
int get_frame_size( char * buf ){
  if ( !buf ) return -1;
  int x = 0, i = 0;
  x = (buf[0] - '0'); // first digit
  
  for(i = 1; i < 5;i++) x = 10*x + ( buf[i] - '0' );
  
  return x;
}

// --------------------------------------
// HELPER
// This function will read the rtsp message and return the
// type of method
enum methods getMethod( char* buff ){
  if( !buff ) return INVALID;
  char temp[BUFFERLENGTH];
  char * temp_buf = buff;
  int i = 0;
  // get the first word
  while( *temp_buf != ' '){
    temp[ i++ ] = *temp_buf;
    temp_buf++;
  }
  // append null at the end
  temp[i] = '\0';
  
  // check what the word says
  if( strcmp( temp, "PLAY" ) == 0 ) return PLAY;
  else if ( strcmp( temp, "PAUSE" ) == 0 ) return PAUSE;
  else if ( strcmp( temp, "TEARDOWN" ) == 0 ) return TEARDOWN;
  else if ( strcmp( temp, "SETUP" ) == 0 ) return SETUP;
  
  return INVALID;
}

// --------------------------------------
// HELPER
// gets the name of the video 
void getVideoName( char* buff, char * dest ){
  if( !buff ) return;
  char * temp_buf = buff;
  int i = 0;
  
  // Skip the first word
  while( *temp_buf != ' ') temp_buf++;
  temp_buf++;
  
  // copy the next word
  // this will have the file name
  while( *temp_buf != ' '){
    dest[ i++ ] = *temp_buf;
    temp_buf++;
  }
  
  // append null at the end
  dest[i] = '\0';
}

// --------------------------------------
// HELPER
// checks if name has "cloud://" in it
// if yes, get rid of it
// and then return true
// otherwise, do nothing
int isCloud( char* name ){
  if( !name ) return -1;
  int i = 0;
  char buffer[BUFFERLENGTH];
  char * temp;
  temp = strstr( name , "cloud://" );
  
  if( temp ) { // name has cloud://
    // copy the name after cloud://
    temp += 8;
    while( *temp != '\0'){
      buffer[i++] = *temp;
      temp++;
    }
    buffer[i++] = '\0';
    memcpy( name, buffer, i );
    return 1;
  }
  else
    return 0;  
}

// --------------------------------------
// HELPER
// combines name and frame into one string
// stores it in dest
void combineNameAndFrame( char* dest, char* name, int frame ){
  // assume dest has enough memory to hold both
  if( !dest || !name || frame < 0 ) return;
  int i = strlen(name);
  char number_buffer[BUFFERLENGTH];
  
  sprintf(number_buffer, "%d", frame); // turn frame into a string
  memcpy(dest, name, i);
  dest[i++] = ':';
  memcpy(dest+i, number_buffer, strlen(number_buffer));
  i += strlen(number_buffer);
  dest[i++] = '\n';
  dest[i] = '\0';
}

// --------------------------------------
// HELPER
// gets the frame rate of the video 
int getFrameSkip( char* buff ){
  if( !buff ) return -1;
  char temp[BUFFERLENGTH];
  char * temp_buf = buff;
  int i = 0;
  int x = 0;
  
  // Get "Scale: x" so we can extract
  // x from the string
  while( *temp_buf != '\n' ){
    temp[ i++ ] = *temp_buf;
    temp_buf++;
  }
  temp[i] = '\0';
  
  sscanf(temp, "Scale: %d", &x);  
  return x;
}

// --------------------------------------
// HELPER
// gets the cseq number 
int getCSeq( char* buff ){
  if( !buff ) return -1;
  char temp[BUFFERLENGTH];
  char * temp_buf;
  int i = 0;
  int x = 0;
  temp_buf = strstr( buff , "CSeq: " );
  if ( !temp_buf ) return -1;
  
  // Get "Scale: x" so we can extract
  // x from the string
  while( *temp_buf != '\n' ){
    temp[ i++ ] = *temp_buf;
    temp_buf++;
  }
  temp[i] = '\0';
  
  sscanf(temp, "CSeq: %d", &x);  
  return x;
}

// --------------------------------------
// HELPER
// returns string that have appended the cseq
// and the session in it
void constructOKResponse( char * temp, int cseq, int session ){
  if( !temp ) return;
  
  char seq [BUFFERLENGTH];
  char sesh[BUFFERLENGTH];
  char * x = "RTSP/1.0 200 OK\0";
  char * seqTag = "\nCSeq: \0";
  char * seshTag = "\nSession: \0";
  
  int i = 0, j = 0;
  while( x[i] != '\0' ){
    temp[i] = x[i]; // copy first line
    i++;
  }
  while( *seqTag != '\0' ){  // copy cseq tag
    temp[i++] = *seqTag;
    seqTag++;
  }
  
  sprintf(seq, "%d", cseq); // copy the actual #
  while( seq[j] != '\0' ) temp[i++] = seq[j++];
  
  while( *seshTag != '\0' ){  // copy sesh tag
    temp[i++] = *seshTag;
    seshTag++;
  }
  
  sprintf(sesh, "%d", session); // copy the actual #
  while( sesh[j] != '\0' ) temp[i++] = sesh[j++];
  temp[i++] = '\n'; // finish up the message
  temp[i++] = '\n';
  temp[i] = '\0';
}

// --------------------------------------
// HELPER
// get sockaddr, IPv4 or IPv6:
void *get_in_addr(struct sockaddr *sa){
  if (sa->sa_family == AF_INET) {
      return &(((struct sockaddr_in*)sa)->sin_addr);
  }
  return &(((struct sockaddr_in6*)sa)->sin6_addr);
}