#ifndef RTSPD_HELPER_H
#define RTSPD_HELPER_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <stdint.h>
 // openCV
#include <cv.h>
#include <highgui.h> 
// threads
#include <pthread.h> 
// timers
#include <sys/time.h> 
#include <signal.h>
#include <time.h>
// locking
#include <semaphore.h>
// cloud stuff
#include "cloud_helper.h"

#define BACKLOG 10     // how many pending connections queue will hold
#define BUFFERLENGTH 1024
#define WIDTH 720
#define HEIGHT 480 
typedef int bool;
sem_t mutex;

// --------------------------------------
// ENUM
// for states
enum rtsp_states {
    INIT, READY, PLAYING
};

// --------------------------------------
// STRUCT
// This struct is created to save information that will be needed by the timer,
// such as socket file descriptors, frame numbers and video captures.
struct send_frame_data {
  int socket_fd;
  uint16_t frames;
  int skip;
  uint32_t timestamp;
  timer_t *play_timer;
  struct itimerspec *play_interval;
  int is_opened;
  enum rtsp_states state;
  char * cloud_server_name;
  char * cloud_video_name;
  int cloud_port;
  int cloud_fd;
  int is_cloud;
  int last_ten_frames_size[10];
  CvCapture* video;
};

// --------------------------------------
// ENUM
// for the type of RTSP message
enum methods {
  SETUP, PLAY, PAUSE, TEARDOWN, INVALID
};

// --------------------------------------
// STRUCT
// This is for RTP packet headers
struct RTP_headers {
    uint16_t first_two; // payload type of jpeg is 26
    uint16_t sequence;
    uint32_t timestamp;
    uint32_t ssrc;
};

// --------------------------------------
// STRUCT
// RTP packet to hold the jpeg to send to clients
struct RTP_packet{
 uint16_t prefix; // first 2 bytes
 uint16_t length; // payload length + 12 bytes for headers
 struct RTP_headers header; // headers
 char payload[]; // need to malloc according to size of 
                 // jpeg and copy the bytes over
};


// --------------------------------------
// HELPER
// compare the names, not 0 if different
int compare_name( char* one, char * two );

// --------------------------------------
// HELPER
// gets the first 5 characters of a given buffer
// turns that into an integer and returns it
int get_frame_size( char * buf );

// --------------------------------------
// HELPER
// This function will be called when the timer ticks
void send_frame(union sigval sv_data);
  
// --------------------------------------
// HELPER
// This function will read the rtsp message and return the
// type of method
enum methods getMethod( char* buff );

// --------------------------------------
// HELPER
// gets the name of the video
void getVideoName( char* buff, char * dest );

// --------------------------------------
// HELPER
// checks if name has "cloud://" in it
// if yes, get rid of it
// and then return true
// otherwise, do nothing to name, return false
int isCloud( char* name );

// --------------------------------------
// HELPER
// combines name and frame into one string
// stores it in dest
void combineNameAndFrame( char* dest, char* name, int frame );

// --------------------------------------
// HELPER
// gets the frame rate of the video 
int getFrameSkip( char* buff );

// --------------------------------------
// HELPER
// gets the cseq number 
int getCSeq( char* buff );

// --------------------------------------
// HELPER
// makes an RTSP OK message
// using cseq and session
void constructOKResponse( char * temp, int cseq, int session );

// --------------------------------------
// MAIN CLIENT THREAD
// for delivering video to clients
void *serve_client(void *ptr);

// --------------------------------------
// HELPER
// get sockaddr, IPv4 or IPv6:
void *get_in_addr(struct sockaddr *sa);

#endif