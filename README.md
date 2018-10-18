(require 2htdp/universe)
(require 2htdp/image)
(require neu-fall18)
;(register "dictionary.ccs.neu.edu")


; A FeedbackString is one of:
; - ""
; - "dislike"
; - "like"
; - "none"
; Interpretation: The feedback that the user gave to the last song played.  The string
; "none" represents that the user gave no feedback, and the string "" represents
; that no feedback has been received yet (i.e., we are playing the first song).
(define FEEDBACKSTRING-EMPTY "")
(define FEEDBACKSTRING-DISLIKE "dislike")
(define FEEDBACKSTRING-LIKE "like")
(define FEEDBACKSTRING-NONE "none")

; (define (feedbackstring-temp fs)
;   (cond
;     [(string=? fs FEEDBACKSTRING-EMPTY) ...]
;     [(string=? fs FEEDBACKSTRING-DISLIKE) ...]
;     [(string=? fs FEEDBACKSTRING-LIKE) ...]
;     [(string=? fs FEEDBACKSTRING-NONE) ...]))


(define-struct song [name artist length album bytes]);
;A Song (make-song String String String Number String)
;Interpretation: has the data to show what song is playing or coming next

(define SONG-1 (make-song "Mona Lisa"
                          "Lil Wayne"
                          33
                          "Carter V"
                          "/Users/pk/Downloads/Lil Wayne ft. Kendrick Lamar- Mona Lisa
(Carter 5).mp3"))

#;
(define (temp-song s)
  (.... (song-name s) .... (song-artist s) ... (song-length s)
        .... (song-album s).... (song-bytes)....))


; A Status is one of
; - 0
; - "receiving"
; - Song
; Interpretation: a status is 0 if there is no request and one must be sent
; "recieving if the request is made and the song is being sent
; and a song that we should play
; recieved it, or we recieve the song after a request

(define STATUS1 0)
(define STATUS2 "receiving")
(define STATUS3 SONG-1)

#;
(define (temp-status stat)
  (cond
    [(number? stat) ...]
    [(string? stat) ...]
    [(song? stat) ... (temp-song stat)]))

(define-struct musicplayer [status feedback])
;A MusicPlayer is a (make-musicplayer Status FeedbackString)
; Interpretation:
;-where the Status is the current status of the MusicPlayer
; - feedback is the feedback received from the user for the last song played
(define MUSICPLAYER-INIT (make-musicplayer 0 "none"))
(define MUSICPLAYER-1 (make-musicplayer "receiving" "like"))
(define MUSICPLAYER-2 (make-musicplayer SONG-1 "dislike"))

#;
(define (temp-musicplayer mp)
  (.... (temp-status (musicplayer-status mp) ... (feedbackstring-temp (musicplayer-feedback mp)))))

; A ServerMsg is one of:
; - ErrorMsg
; - SongMsg
 
; An ErrorMsg is a (list "ERROR" String)
; where the second string is the message from the server about what went wrong
 
; A SongMsg is a (list "SONG" Metadata String)
; - where the metadata is information about the given song
; - and the second the String is the actual byte-string of music
 
; A Metadata is a (list String String Number String)
; - where the first String is the song's title
; - the second String is the song's artist
; - the Number is the length of the song (in seconds)
; - and the third String is the song's album

; A Package is a (make-package MusicPlayer ClientMsg)
; - and dictacts the next state of the world as well as
; - the message sent from the client to the server
 
; A ClientMsg is "next"
; and represents a request for the next song

; Main-Player : MusicPlayer -> MusicPlayer
; World Program for a MusicPlayer





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-draw
;Constants for drawing
(define BACKGROUND (empty-scene 1250 1250))
(define TEXT-NEXT-SONG "Next song: ")
(define TEXT-NEXT-ARTIST "Artist: ")
(define TEXT-INIT "Requesting song. Feedback of last song: ")
(define TEXT-RECIEVING "Recieving song. Feedback of last song: ")
(define TEXT-HAS-SONG "Press space to play the song. Feedback of last song: ")
(define TEXT-SIZE 28)
(define TEXT-COLOR "black")


; Draw-Music : MusicPlayer -> Image
; Draws the state of the world


(check-expect (draw-music MUSICPLAYER-INIT)
              (overlay (text (string-append TEXT-INIT "none") TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(check-expect (draw-music MUSICPLAYER-1)
              (overlay (text (string-append TEXT-RECIEVING "like")
                             TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(check-expect (draw-music MUSICPLAYER-2)
              (overlay (text (string-append TEXT-HAS-SONG "dislike"
                                            TEXT-NEXT-SONG "Mona Lisa"
                                            TEXT-NEXT-ARTIST "Lil Wayne")
                             TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))


(define (draw-music mp)
  (draw-music-player (musicplayer-status mp) mp))

; draw-music-player : Status MusicPlayer -> Image
; takes in a status and creates an image

(check-expect (draw-music-player 0 MUSICPLAYER-INIT)
              (overlay (text (string-append TEXT-INIT "none") TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(check-expect (draw-music-player "receiving" MUSICPLAYER-1)
              (overlay (text (string-append TEXT-RECIEVING "like")
                             TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(check-expect (draw-music-player SONG-1 MUSICPLAYER-2)
              (overlay (text (string-append TEXT-HAS-SONG "dislike"
                                            TEXT-NEXT-SONG "Mona Lisa"
                                            TEXT-NEXT-ARTIST "Lil Wayne")
                             TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(define (draw-music-player s mp)
  (cond
    [(number? s) (draw-music-player-request mp)]
    [(string? s) (draw-music-player-receiving mp)]
    [(song? s) (draw-music-player-song mp)]))

; draw-music-player-request: MusicPlayer -> Image
; draws the Image for the world when a song is being requested

(check-expect (draw-music-player-request MUSICPLAYER-INIT)
              (overlay (text (string-append TEXT-INIT "none") TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(define (draw-music-player-request mp)
  (overlay (text (string-append TEXT-INIT (musicplayer-feedback mp)) TEXT-SIZE TEXT-COLOR)
           BACKGROUND))

;draw-music-player-receiving: MusicPlayer -> Image
;draws the Image for the world when a song is being requested

(check-expect (draw-music-player-receiving MUSICPLAYER-1)
              (overlay (text (string-append TEXT-RECIEVING "like")
                             TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(define (draw-music-player-receiving mp)
  (overlay (text (string-append TEXT-RECIEVING (musicplayer-feedback mp)) TEXT-SIZE TEXT-COLOR)
           BACKGROUND))

;draw-music-player-song: MusicPlayer -> Image
;draws the Image for the world when a song has been sent

(check-expect (draw-music-player-song MUSICPLAYER-2)
              (overlay (text (string-append TEXT-HAS-SONG "dislike"
                                            TEXT-NEXT-SONG "Mona Lisa"
                                            TEXT-NEXT-ARTIST "Lil Wayne")
                             TEXT-SIZE TEXT-COLOR)
                       BACKGROUND))

(define (draw-music-player-song mp)
  (overlay (text (string-append TEXT-HAS-SONG (musicplayer-feedback mp)
                                TEXT-NEXT-SONG (song-name (musicplayer-status mp))
                                TEXT-NEXT-ARTIST (song-artist (musicplayer-status mp)))
                 TEXT-SIZE TEXT-COLOR)
           BACKGROUND))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; on-recieve


; A Package is a (make-package MusicPlayer ClientMsg)
; - and dictacts the next state of the world as well as
; - the message sent from the client to the server


 
; A ClientMsg is "next"
; and represents a request for the next song

;;;;;;;;;;;;;;;;;;;; on-key

;play-song: MusicPlayer Keyevent -> MusicPlayer
; plays the song when there is a song to play and space bar has been hit

(check-expect (play-song MUSICPLAYER-INIT " ") MUSICPLAYER-INIT)
(check-expect (play-song MUSICPLAYER-1 " ") MUSICPLAYER-1)
(check-expect (play-song MUSICPLAYER-2 "r") MUSICPLAYER-2)
;NO CHECK EXPECT FOR WHEN SONG PLAYS AS IT USES BIG BANG

(define (play-song mp ke)
  (cond
    [(string=? " " ke) (play-song-space mp)]
    [else mp]))


; play-song-space: MusicPlayer -> MusicPlayer
; retrives the status

(check-expect (play-song-space MUSICPLAYER-INIT) MUSICPLAYER-INIT)
(check-expect (play-song-space MUSICPLAYER-1) MUSICPLAYER-1)


(define (play-song-space mp)
  (status-play-song (musicplayer-status mp) mp))

; status-play-song : Status MusicPlayer-> MusicPlayer
; plays song if music player is right

(check-expect (status-play-song 0 MUSICPLAYER-INIT) MUSICPLAYER-INIT)
(check-expect (status-play-song "recieved" MUSICPLAYER-1) MUSICPLAYER-1)

(define (status-play-song stat mp)
  (cond
    [(number? stat) mp]
    [(string? stat) mp]
    [(song? stat) (make-musicplayer 0 (play-sound stat))]))

; make in the function it goes back to 


;;;;;; on-tick
; A PlayerResult is one of:
; - MusicPlayer
; - Package

; Request-Song ; MusicPlayer -> PlayerResult
; Requests a song if needed

(check-expect (request-song MUSICPLAYER-INIT) (make-package MUSICPLAYER-INIT "next"))
(check-expect (request-song MUSICPLAYER-1) MUSICPLAYER-1)
(check-expect (request-song MUSICPLAYER-2) MUSICPLAYER-2)


(define (request-song mp)
  (cond
    [(number? (musicplayer-status mp)) (make-package mp "next")]
    [else mp]))




;;;;;;;;;;;;;;;;;; on-receive

(define ERROR-MESSAGE "a package with a Music Player was not sent by the server")
; Get-Song : MusicPlayer ServerMsg -> PlayerResult
; Recieves back information from the server

(define (get-song mp msg)
  (cond
    [(string=? (first msg) "ERROR") ERROR-MESSAGE]
    [(string=? (first msg) "SONG") (to-musicplayer (second msg) (third msg) mp)]))

; To-MusicPlayer: MetaData String -> MusicPlayer
; extracts the information from MetaData and the bytes of the song, and places this in
; the Music Player

(define (to-musicplayer m b mp)
  (make-musicplayer (extract-song m b) (musicplayer-feedback mp)))

; extract-song: MetaData String -> Song
; places the information from the ServerMsg into the song

(define (extract-song m b)
  (make-song (first m) (second m) (third m) (fourth m) b))
       

(define (main-music-player mp-init)
(big-bang mp-init
[port 10001]
[register "dictionary.ccs.neu.edu"]
 [to-draw draw-music]
 [on-key play-song]
 [on-tick request-song]
 [on-receive get-song]))


