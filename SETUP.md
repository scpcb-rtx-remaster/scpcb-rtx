# Setting up

### How to add new rooms:

- first you need to add the information of the room in Data\rooms.ini
	- "mesh path" is the path of the room model
	- shape determines the layout of the room:
		- 1 = a "dead end room" with only one door leading to it
		- 2 = a room with two doors opposite to each other (a hallway)
		- 2C = a "corner room" with two doors, like the lockrooms
		- 3 = a T-shaped room with three doors
		- 4 = a room with four doors, such as the "room4tunnels" -room
	- commoness determines how often the map generation algorithm uses the room
	if you want the room to appear only once per map, set the commonness-value 
	to 0 and place the room into the map by adding it into the "CreateMap"-function
	in the "MapSystem.bb"-file

- if you need to place items, triggers, additional doors or other things into the room,
see the "FillRoom"-function the "MapSystem.bb"-file

----------------------------------------------------------------------------------------

### How to add new events:

- Events are placed into rooms in the "InitEvents"-function in the "Main.bb"-file.
- You can add a new event using the "CreateEvent"-function. The function has the following
parameters:
	- eventname$ = the name of the event. It can be anything, as long as you're using
	the same name in the UpdateEvents-function
	- roomname$ = the event is only assigned to rooms with this name
	- id% = determines which of the rooms the event is assigned to:
		- 0 will assign it to the first generated room, 1 to the second, etc	
	- prob# = can be used to randomly assign events into some rooms
		- 0.5 means that there's a 50% chance that event is assigned to the rooms
		- 1.0 means that the event is assigned to every room
		- the id%-variable is ignored if prob# > 0.0
		- the prob#-variable is ignored if it's value is 0.0

- Events are updated in the "UpdateEvents"-function. See the existing events to get an idea
of how the events work.

- The events-type has the following fields which you can use when updating the event:
	- room = the room which the event has been assigned to
		- the room-field can be used to gain access to the objects, doors and
		triggers in the room. For instance, you could add a pivot to the room
		and store its handle in the Objects[n]-field of the room type, and access it
		with event\room\objects[n]
	- Eventstate#, Eventstate2#, Eventstate3# = Just three float variables which can
	be used as timers for example
	- Sound% = a variable for storing a sound clip, to make it easier to free the clip
	when it is no longer needed
	- SoundCHN%, SoundCHN2% = These can be used to track whether a sound clip is playing
	(using "ChannelPlaying"-function). They are also automatically paused when the
	inventory or the menu is opened.
	- img% = a variable for storing an image, if one is needed in the event

