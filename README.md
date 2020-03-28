# MoreMethods
Improves VScripting in L4D2 by inserting new methods into base classes

Simply include the MoreMethods script, the methods will be safe to use when the MoreMethods_Finished variable is true.

# Documentation

### General Methods
| Function Name | Details |
| --- | --- |
HasProp|   			     function(propertyName) - Returns true if the entity has the specified NetProp, false otherwise.
GetPropType|			     function(propertyName) - Returns the NetProp type.
GetPropArraySize|     function(propertyName) - Returns the array size of the NetProp.
GetPropInt|			     function(propertyName) - Returns the NetProp as an integer.
GetPropEntity|		     function(propertyName) - Returns the NetProp as an entity.
GetPropString|		     function(propertyName) - Returns the NetProp as a string.
GetPropFloat|		     function(propertyName) - Returns the NetProp as a float.
GetPropVector|		     function(propertyName) - Returns the NetProp as a Vector.		
SetPropInt|			     function(propertyName, value) - Sets the NetProp to an integer.
SetPropEntity|		     function(propertyName, value) - Sets the NetProp to an entity.
SetPropString|		     function(propertyName, value) - Sets the NetProp to a string.
SetPropFloat|		     function(propertyName, value) - Sets the NetProp to a float.
SetPropVector|		     function(propertyName, value) - Sets the NetProp to a Vector.		
GetPropIntArray|		   function(propertyName, index) - Returns the integer value in the index in the NetProp array.
GetPropEntityArray|	 function(propertyName, index) - Returns the entity value in the index in the NetProp array.
GetPropStringArray|	 function(propertyName, index) - Returns the string value in the index in the NetProp array.
GetPropFloatArray|	   function(propertyName, index) - Returns the float value in the index in the NetProp array.
GetPropVectorArray|	 function(propertyName, index) - Returns the Vector value in the index in the NetProp array.		
SetPropIntArray|		  = function(propertyName, index, value) - Sets the index in the NetProp array to an integer.
SetPropEntityArray|	 function(propertyName, index, value) - Sets the index in the NetProp array to an entity.
SetPropStringArray|	 function(propertyName, index, value) - Sets the index in the NetProp array to a string.
SetPropFloatArray|	   function(propertyName, index, value) - Sets the index in the NetProp array to a float.
SetPropVectorArray|	 function(propertyName, index, value) - Sets the index in the NetProp array to a Vector.		
SetProp|				       function(propertyName, value, index = null) - Sets the NetProp to the provided value.
GetProp|				       function(propertyName, index = null, asArray = false) - Gets the NetProp. Will return an array of the values if asArray is true. WARNING: This cannot return entity handles, use GetPropEntity if you know the NetProp is an entity.
SetRenderFX|			     function(value) - Set the render effects
GetRenderFX|			     function() - Get the render effects		
SetRenderMode|		     function(value) - Set the rendermode
GetRenderMode|		     function() - Get the rendermode		
GetModelIndex|		     function() - Get the model index
GetModelName|		     function() - Get the model name		
SetName|				       function(name) - Set the targetname		
GetFriction|			     function() - Get the friction 		
GetFlags|			       function() - Get entity flags
SetFlags|			       function(flag) - Set entity flags
AddFlag|				      = function(flag) - Add a specific flag
RemoveFlag|			     function(flag) - Remove a specific flag
HasFlag|				      = function(flag) - Returns true if the entity has the flag		
GetMoveType|			     function() - Get the movetype
SetMoveType|			     function(type) - Set the movetype		
GetGlowType|			     function() - Get the glow type
SetGlowType|			     function(type) - Set the glow type		
GetGlowRange|		     function() - Get the glow range
SetGlowRange|		     function(range) - Set the glow range		
GetGlowRangeMin|		   function() - Get the minimum glow range
SetGlowRangeMin|		   function(range) - Set the minimum glow range
GetGlowColor|		     function() - Get the glow color as an integer
SetGlowColor|		     function(r, g, b) - Set the glow color
SetGlowColorVector|	 function(vector) - Set the glow color with a Vector
ResetGlowColor|		   function() - Reset glow color		
GetTeam|				       function() - Get the team number
SetTeam|				       function(team) - Set the team number		
GetGlowFlashing|		   function() - Returns true if the glow is set to flash
SetGlowFlashing|		   function(flashing) - Sets the glow to flash		
GetFlowDistance|		   function() - Get the flow distance
GetFlowPercent|		   function() - Get the flow percent		
PlaySound|			       function(soundName) - Play a sound centered on this entity
StopSound|			       function(soundName) - Stop a sound		
Input|				         function(input, value = "", delay = 0, activator = null) - Fire an input
SetAlpha|			       function(alpha) - Set alpha
Enable|				       function() - Enable this entity
Disable|				       function() - Disable this entity
GetValidatedScriptScope|	 function() - Validate script scope and return it		
SetModelScale| 		   function(modelScale) - Set the model scale
GetModelScale| 		   function() - Get the model scale
