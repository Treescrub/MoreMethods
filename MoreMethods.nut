/* Copyright Notice
	Copyright (c) 2019 Daroot Leafstorm

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
 */

/* IMPORTANT USAGE INFO
	Using any of these methods before OnGameplayStart is called is undefined behavior and may result in an error.
	To check if you are able to use the extra methods without errors, check the MoreMethods_Finished variable in the root table.
*/

::MoreMethods_Finished <- false

::MoreMethods_Include <- function(){
	printl("Including MoreMethods at server time " + Time())
	local shared_functions = {
		HasProp 			= function(propertyName){return NetProps.HasProp(this, propertyName)}
		GetPropType			= function(propertyName){return NetProps.GetPropType(this, propertyName)}
		GetPropArraySize	= function(propertyName){return NetProps.GetPropArraySize(this, propertyName)}
		
		GetPropInt			= function(propertyName){return NetProps.GetPropInt(this, propertyName)}
		GetPropEntity		= function(propertyName){return NetProps.GetPropEntity(this, propertyName)}
		GetPropString		= function(propertyName){return NetProps.GetPropString(this, propertyName)}
		GetPropFloat		= function(propertyName){return NetProps.GetPropFloat(this, propertyName)}
		GetPropVector		= function(propertyName){return NetProps.GetPropVector(this, propertyName)}
		
		SetPropInt			= function(propertyName, value){NetProps.SetPropInt(this, propertyName, value)}
		SetPropEntity		= function(propertyName, value){NetProps.SetPropEntity(this, propertyName, value)}
		SetPropString		= function(propertyName, value){NetProps.SetPropString(this, propertyName, value)}
		SetPropFloat		= function(propertyName, value){NetProps.SetPropFloat(this, propertyName, value)}
		SetPropVector		= function(propertyName, value){NetProps.SetPropVector(this, propertyName, value)}
		
		GetPropIntArray		= function(propertyName, index){return NetProps.GetPropIntArray(this, propertyName, index)}
		GetPropEntityArray	= function(propertyName, index){return NetProps.GetPropEntityArray(this, propertyName, index)}
		GetPropStringArray	= function(propertyName, index){return NetProps.GetPropStringArray(this, propertyName, index)}
		GetPropFloatArray	= function(propertyName, index){return NetProps.GetPropFloatArray(this, propertyName, index)}
		GetPropVectorArray	= function(propertyName, index){return NetProps.GetPropVectorArray(this, propertyName, index)}
		
		SetPropIntArray		= function(propertyName, index, value){NetProps.SetPropIntArray(this, propertyName, value, index)}
		SetPropEntityArray	= function(propertyName, index, value){NetProps.SetPropEntityArray(this, propertyName, value, index)}
		SetPropStringArray	= function(propertyName, index, value){NetProps.SetPropStringArray(this, propertyName, value, index)}
		SetPropFloatArray	= function(propertyName, index, value){NetProps.SetPropFloatArray(this, propertyName, value, index)}
		SetPropVectorArray	= function(propertyName, index, value){NetProps.SetPropVectorArray(this, propertyName, value, index)}
		
		SetProp				= function(propertyName, value, index = null){
								if(GetPropType(propertyName) == "integer"){
									if(GetPropArraySize(propertyName) > 1){
										if(typeof(value) == "array"){
											for(local i=0; i < value.len(); i++){
												SetPropIntArray(propertyName, i, value[i])
											}
										} else {
											SetPropIntArray(propertyName, index, value)
										}
									} else {
										if(typeof(value) == "instance"){
											SetPropEntity(propertyName, value)
										} else {
											SetPropInt(propertyName, value)
										}
									}
								} else if(GetPropType(propertyName) == "float"){
									if(GetPropArraySize(propertyName) > 1){
										if(typeof(value) == "array"){
											for(local i=0; i < value.len(); i++){
												SetPropFloatArray(propertyName, i, value[i])
											}
										} else {
											SetPropFloatArray(propertyName, index, value)
										}
									} else {
										SetPropFloat(propertyName, value)
									}
								} else if(GetPropType(propertyName) == "Vector"){
									if(GetPropArraySize(propertyName) > 1){
										if(typeof(value) == "array"){
											for(local i=0; i < value.len(); i++){
												SetPropVectorArray(propertyName, i, value[i])
											}
										} else {
											SetPropVectorArray(propertyName, index, value)
										}
									} else {
										SetPropVector(propertyName, value)
									}
								} else if(GetPropType(propertyName) == "string"){
									if(GetPropArraySize(propertyName) > 1){
										if(typeof(value) == "array"){
											for(local i=0; i < value.len(); i++){
												SetPropStringArray(propertyName, i, value[i])
											}
										} else {
											SetPropStringArray(propertyName, index, value)
										}
									} else {
										SetPropString(propertyName, value)
									}
								}
							}
		GetProp				= function(propertyName, index = null, asArray = false){
								if(GetPropType(propertyName) == "integer"){
									if(GetPropArraySize(propertyName) > 1){
										if(asArray){
											local netpropArray = []
											for(local i=0; i < GetPropArraySize(propertyName); i++){
												netpropArray.append(GetPropIntArray(propertyName, i))
											}
											return netpropArray
										}
										return GetPropIntArray(propertyName, index)
									} else {
										return GetPropInt(propertyName)
									}
								} else if(GetPropType(propertyName) == "float"){
									if(GetPropArraySize(propertyName) > 1){
										if(asArray){
											local netpropArray = []
											for(local i=0; i < GetPropArraySize(propertyName); i++){
												netpropArray.append(GetPropFloatArray(propertyName, i))
											}
											return netpropArray
										}
										return GetPropFloatArray(propertyName, index)
									} else {
										return GetPropFloat(propertyName)
									}
								} else if(GetPropType(propertyName) == "Vector"){
									if(GetPropArraySize(propertyName) > 1){
										if(asArray){
											local netpropArray = []
											for(local i=0; i < GetPropArraySize(propertyName); i++){
												netpropArray.append(GetPropVectorArray(propertyName, i))
											}
											return netpropArray
										}
										return GetPropVectorArray(propertyName, index)
									} else {
										return GetPropVector(propertyName)
									}
								} else if(GetPropType(propertyName) == "string"){
									if(GetPropArraySize(propertyName) > 1){
										if(asArray){
											local netpropArray = []
											for(local i=0; i < GetPropArraySize(propertyName); i++){
												netpropArray.append(GetPropStringArray(propertyName, i))
											}
											return netpropArray
										}
										return GetPropStringArray(propertyName, index)
									} else {
										return GetPropString(propertyName)
									}
								}
							}
							
		SetRenderFX			= function(value){SetProp("m_nRenderFX", value.tointeger())}
		GetRenderFX			= function(){return GetProp("m_nRenderFX")}
		
		SetRenderMode		= function(value){SetProp("m_nRenderMode", value.tointeger())}
		GetRenderMode		= function(){return GetProp("m_nRenderMode")}
		
		GetModelIndex		= function(){return GetPropInt("m_nModelIndex")}
		GetModelName		= function(){return GetPropString("m_ModelName")}
		
		SetName				= function(name){SetPropString("m_iName", name)}
		
		GetFriction			= function(){return GetFriction(this)}
		
		GetFlags			= function(){return GetPropInt("m_fFlags")}
		SetFlags			= function(flag){SetPropInt("m_fFlags", flag)}
		AddFlag				= function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") | flag)}
		RemoveFlag			= function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") & ~flag)}
		HasFlag				= function(flag){return (GetFlags() & flag) != 0}
		
		GetMoveType			= function(){return GetPropInt("movetype")}
		SetMoveType			= function(type){SetPropInt("movetype", type)}
		
		GetGlowType			= function(){return GetPropInt("m_Glow.m_iGlowType")}
		SetGlowType			= function(type){SetPropInt("m_Glow.m_iGlowType", type)}
		
		GetGlowRange		= function(){return GetPropInt("m_Glow.m_nGlowRange")}
		SetGlowRange		= function(range){SetPropInt("m_Glow.m_nGlowRange", range)}
		
		GetGlowRangeMin		= function(){return GetPropInt("m_Glow.m_nGlowRangeMin")}
		SetGlowRangeMin		= function(range){SetPropInt("m_Glow.m_nGlowRangeMin", range)}
		
		GetGlowColor		= function(){return GetPropInt("m_Glow.m_glowColorOverride")}
		SetGlowColor		= function(r, g, b){
								local color = r
								color += 256 * g
								color += 65536 * b
								SetPropInt("m_Glow.m_glowColorOverride", color)
							}
		SetGlowColorVector	= function(vector){
								local color = vector.x
								color += 256 * vector.y
								color += 65536 * vector.z
								SetPropInt("m_Glow.m_glowColorOverride", color)
							}
		ResetGlowColor		= function(){SetPropInt("m_Glow.m_glowColorOverride", -1)}
		
		SetTeam				= function(team){SetPropInt("m_iTeamNum", team.tointeger())}
		GetTeam				= function(){return GetPropInt("m_iTeamNum")}
		
		GetGlowFlashing		= function(){return GetPropInt("m_Glow.m_bFlashing")}
		SetGlowFlashing		= function(flashing){SetPropInt("m_Glow.m_bFlashing", flashing)}
		
		GetFlowDistance		= function(){return GetFlowDistanceForPosition(GetOrigin())}
		GetFlowPercent		= function(){return GetFlowPercentForPosition(GetOrigin())}
		
		PlaySound			= function(soundName){EmitSoundOn(soundName, this)}
		StopSound			= function(soundName){StopSoundOn(soundName, this)}
		
		Input				= function(input, value = "", delay = 0, activator = null){DoEntFire("!self", input.tostring(), value.tostring(), delay.tofloat(), activator, this)}
		SetAlpha			= function(alpha){Input("Alpha", alpha)}
		Enable				= function(){Input("Enable")}
		Disable				= function(){Input("Disable")}
		GetValidatedScriptScope	= function(){ValidateScriptScope();return GetScriptScope()}
	}

	local entity_animating_shared_functions = {
		GetSpawnflags = function(){return GetPropInt("m_spawnflags")}
		SetSpawnFlags = function(flags){SetPropInt("m_spawnflags", flags)}
	}

	foreach(key, val in shared_functions){
		CBaseEntity[key] <- val
		CBaseAnimating[key] <- val
		CTerrorPlayer[key] <- val
	}
	
	foreach(key, val in entity_animating_shared_functions){
		CBaseEntity[key] <- val
		CBaseAnimating[key] <- val
	}

	CBaseAnimating["SetRenderFX"] <- function(value){SetProp("m_nRenderFX", value.tointeger())}
	CBaseAnimating["GetRenderFX"] <- function(){return GetProp("m_nRenderFX")}

	CBaseAnimating["SetRenderMode"] <- function(value){SetProp("m_nRenderMode", value.tointeger())}
	CBaseAnimating["GetRenderMode"] <- function(){return GetProp("m_nRenderMode")}

	CBaseAnimating["SetClip"] <- function(clip){SetPropInt("m_iClip1", clip)}
	CBaseAnimating["GetClip"] <- function(){return GetPropInt("m_iClip1")}
	CBaseAnimating["SetReserveAmmo"] <- function(ammo){SetPropInt("m_iExtraPrimaryAmmo", ammo)}
	CBaseAnimating["GetReserveAmmo"] <- function(){return GetPropInt("m_iExtraPrimaryAmmo")}

	CBaseAnimating["SetUpgradedAmmoLoaded"] <- function(value){SetProp("m_nUpgradedPrimaryAmmoLoaded", value.tointeger())}
	CBaseAnimating["GetUpgradedAmmoLoaded"] <- function(){return GetProp("m_nUpgradedPrimaryAmmoLoaded")}

	CBaseAnimating["SetUpgrades"] <- function(value){SetProp("m_upgradeBitVec", value.tointeger())}
	CBaseAnimating["GetUpgrades"] <- function(){return GetProp("m_upgradeBitVec")}


	local initializeCTerrorPlayer = null
	while(initializeCTerrorPlayer = Entities.FindByClassname(initializeCTerrorPlayer, "player")){
		initializeCTerrorPlayer.GetOrigin()
		break
	}

	CTerrorPlayer["SetDucked"] <- function(value){SetProp("m_Local.m_bDucked", value.tointeger())}
	CTerrorPlayer["GetDucked"] <- function(){return GetProp("m_Local.m_bDucked")}

	CTerrorPlayer["SetDucking"] <- function(value){SetProp("m_Local.m_bDucking", value.tointeger())}
	CTerrorPlayer["GetDucking"] <- function(){return GetProp("m_Local.m_bDucking")}

	CTerrorPlayer["SetInDuckJump"] <- function(value){SetProp("m_Local.m_bInDuckJump", value.tointeger())}
	CTerrorPlayer["GetInDuckJump"] <- function(){return GetProp("m_Local.m_bInDuckJump")}

	CTerrorPlayer["SetDuckTimeInMilliseconds"] <- function(value){SetProp("m_Local.m_nDuckTimeMsecs", value.tointeger())}
	CTerrorPlayer["GetDuckTimeInMilliseconds"] <- function(){return GetProp("m_Local.m_nDuckTimeMsecs")}

	CTerrorPlayer["SetDuckJumpTimeInMilliseconds"] <- function(value){SetProp("m_Local.m_nDuckJumpTimeMsecs", value.tointeger())}
	CTerrorPlayer["GetDuckJumpTimeInMilliseconds"] <- function(){return GetProp("m_Local.m_nDuckJumpTimeMsecs")}

	CTerrorPlayer["SetJumpTimeInMilliseconds"] <- function(value){SetProp("m_Local.m_nJumpTimeMsecs", value.tointeger())}
	CTerrorPlayer["GetJumpTimeInMilliseconds"] <- function(){return GetProp("m_Local.m_nJumpTimeMsecs")}

	CTerrorPlayer["SetPunchAngle"] <- function(value){SetProp("m_Local.m_vecPunchAngle", value)}
	CTerrorPlayer["GetPunchAngle"] <- function(){return GetProp("m_Local.m_vecPunchAngle")}

	CTerrorPlayer["SetPunchAngleVel"] <- function(value){SetProp("m_Local.m_vecPunchAngleVel", value)}
	CTerrorPlayer["GetPunchAngleVel"] <- function(){return GetProp("m_Local.m_vecPunchAngleVel")}

	CTerrorPlayer["SetAllowAutoMovement"] <- function(value){SetProp("m_Local.m_bAllowAutoMovement", value.tointeger())}
	CTerrorPlayer["GetAllowAutoMovement"] <- function(){return GetProp("m_Local.m_bAllowAutoMovement")}

	CTerrorPlayer["SetAutoAimTarget"] <- function(value){SetProp("m_Local.m_bAutoAimTarget", value.tointeger())}
	CTerrorPlayer["GetAutoAimTarget"] <- function(){return GetProp("m_Local.m_bAutoAimTarget")}

	CTerrorPlayer["SetLastWeapon"] <- function(value){SetPropEntity("m_Local.m_hLastWeapon", value)}
	CTerrorPlayer["GetLastWeapon"] <- function(){return GetPropEntity("m_Local.m_hLastWeapon")}

	CTerrorPlayer["SetDeathTime"] <- function(value){SetProp("m_Local.m_flDeathTime", value.tofloat())}
	CTerrorPlayer["GetDeathTime"] <- function(){return GetProp("m_Local.m_flDeathTime")}

	CTerrorPlayer["SetTonemapController"] <- function(value){SetPropEntity("m_Local.m_hTonemapController", value)}
	CTerrorPlayer["GetTonemapController"] <- function(){return GetPropEntity("m_Local.m_hTonemapController")}

	CTerrorPlayer["SetShotsFired"] <- function(value){SetProp("m_iShotsFired", value.tointeger())}
	CTerrorPlayer["GetShotsFired"] <- function(){return GetProp("m_iShotsFired")}

	CTerrorPlayer["SetVelocityModifier"] <- function(value){SetProp("m_flVelocityModifier", value.tofloat())}
	CTerrorPlayer["GetVelocityModifier"] <- function(){return GetProp("m_flVelocityModifier")}

	CTerrorPlayer["SetArmorValue"] <- function(value){SetProp("m_ArmorValue", value.tointeger())}
	CTerrorPlayer["GetArmorValue"] <- function(){return GetProp("m_ArmorValue")}

	CTerrorPlayer["SetProgressBarDuration"] <- function(value){SetProp("m_flProgressBarDuration", value.tofloat())}
	CTerrorPlayer["GetProgressBarDuration"] <- function(){return GetProp("m_flProgressBarDuration")}

	CTerrorPlayer["SetProgressBarStartTime"] <- function(value){SetProp("m_flProgressBarStartTime", value.tofloat())}
	CTerrorPlayer["GetProgressBarStartTime"] <- function(){return GetProp("m_flProgressBarStartTime")}

	CTerrorPlayer["SetRagdoll"] <- function(value){SetPropEntity("m_hRagdoll", value)}
	CTerrorPlayer["GetRagdoll"] <- function(){return GetPropEntity("m_hRagdoll")}

	CTerrorPlayer["SetInMissionStartArea"] <- function(value){SetProp("m_isInMissionStartArea", value.tointeger())}
	CTerrorPlayer["GetInMissionStartArea"] <- function(){return GetProp("m_isInMissionStartArea")}

	CTerrorPlayer["SetCulling"] <- function(value){SetProp("m_isCulling", value.tointeger())}
	CTerrorPlayer["GetCulling"] <- function(){return GetProp("m_isCulling")}

	CTerrorPlayer["SetRelocating"] <- function(value){SetProp("m_isRelocating", value.tointeger())}
	CTerrorPlayer["GetRelocating"] <- function(){return GetProp("m_isRelocating")}

	CTerrorPlayer["SetGhostSpawnState"] <- function(value){SetProp("m_ghostSpawnState", value.tointeger())}
	CTerrorPlayer["GetGhostSpawnState"] <- function(){return GetProp("m_ghostSpawnState")}

	CTerrorPlayer["SetGhostSpawnClockMaxDelay"] <- function(value){SetProp("m_ghostSpawnClockMaxDelay", value.tointeger())}
	CTerrorPlayer["GetGhostSpawnClockMaxDelay"] <- function(){return GetProp("m_ghostSpawnClockMaxDelay")}

	CTerrorPlayer["SetGhostSpawnClockCurrentDelay"] <- function(value){SetProp("m_ghostSpawnClockCurrentDelay", value.tointeger())}
	CTerrorPlayer["GetGhostSpawnClockCurrentDelay"] <- function(){return GetProp("m_ghostSpawnClockCurrentDelay")}

	CTerrorPlayer["SetNextShoveTime"] <- function(value){SetProp("m_flNextShoveTime", value.tofloat())}
	CTerrorPlayer["GetNextShoveTime"] <- function(){return GetProp("m_flNextShoveTime")}

	CTerrorPlayer["SetShovePenalty"] <- function(value){SetProp("m_iShovePenalty", value.tointeger())}
	CTerrorPlayer["GetShovePenalty"] <- function(){return GetProp("m_iShovePenalty")}

	CTerrorPlayer["SetAutoCrouchEnabled"] <- function(value){SetProp("m_isAutoCrouchEnabled", value.tointeger())}
	CTerrorPlayer["GetAutoCrouchEnabled"] <- function(){return GetProp("m_isAutoCrouchEnabled")}

	CTerrorPlayer["SetAutoCrouchTimestamp"] <- function(value){SetProp("m_autoCrouchTimer.m_timestamp", value.tofloat())}
	CTerrorPlayer["GetAutoCrouchTimestamp"] <- function(){return GetProp("m_autoCrouchTimer.m_timestamp")}

	CTerrorPlayer["SetAdrenalineActive"] <- function(value){SetProp("m_bAdrenalineActive", value.tointeger())}
	CTerrorPlayer["GetAdrenalineActive"] <- function(){return GetProp("m_bAdrenalineActive")}

	CTerrorPlayer["SetZombieClass"] <- function(value){SetProp("m_zombieClass", value.tointeger())}
	CTerrorPlayer["GetZombieClass"] <- function(){return GetProp("m_zombieClass")}

	CTerrorPlayer["SetZombieState"] <- function(value){SetProp("m_zombieState", value.tointeger())}
	CTerrorPlayer["GetZombieState"] <- function(){return GetProp("m_zombieState")}

	CTerrorPlayer["SetMaxDeadDuration"] <- function(value){SetProp("m_maxDeadDuration", value.tofloat())}
	CTerrorPlayer["GetMaxDeadDuration"] <- function(){return GetProp("m_maxDeadDuration")}

	CTerrorPlayer["SetTotalDeadDuration"] <- function(value){SetProp("m_totalDeadDuration", value.tofloat())}
	CTerrorPlayer["GetTotalDeadDuration"] <- function(){return GetProp("m_totalDeadDuration")}

	CTerrorPlayer["SetHangTimestamp"] <- function(value){SetProp("m_hangTimer.m_timestamp", value.tofloat())}
	CTerrorPlayer["GetHangTimestamp"] <- function(){return GetProp("m_hangTimer.m_timestamp")}

	CTerrorPlayer["SetHangAirPos"] <- function(value){SetProp("m_hangAirPos", value)}
	CTerrorPlayer["GetHangAirPos"] <- function(){return GetProp("m_hangAirPos")}

	CTerrorPlayer["SetHangPos"] <- function(value){SetProp("m_hangPos", value)}
	CTerrorPlayer["GetHangPos"] <- function(){return GetProp("m_hangPos")}

	CTerrorPlayer["SetHangStandPos"] <- function(value){SetProp("m_hangStandPos", value)}
	CTerrorPlayer["GetHangStandPos"] <- function(){return GetProp("m_hangStandPos")}

	CTerrorPlayer["SetHangNormal"] <- function(value){SetProp("m_hangNormal", value)}
	CTerrorPlayer["GetHangNormal"] <- function(){return GetProp("m_hangNormal")}

	CTerrorPlayer["SetKnockdownReason"] <- function(value){SetProp("m_knockdownReason", value.tointeger())}
	CTerrorPlayer["GetKnockdownReason"] <- function(){return GetProp("m_knockdownReason")}

	CTerrorPlayer["SetKnockdownTimestamp"] <- function(value){SetProp("m_knockdownTimer.m_timestamp", value.tofloat())}
	CTerrorPlayer["GetKnockdownTimestamp"] <- function(){return GetProp("m_knockdownTimer.m_timestamp")}

	CTerrorPlayer["SetStaggerTimestamp"] <- function(value){SetProp("m_staggerTimer.m_timestamp", value.tofloat())}
	CTerrorPlayer["GetStaggerTimestamp"] <- function(){return GetProp("m_staggerTimer.m_timestamp")}

	CTerrorPlayer["SetStaggerDuration"] <- function(value){SetProp("m_staggerTimer.m_duration", value.tofloat())}
	CTerrorPlayer["GetStaggerDuration"] <- function(){return GetProp("m_staggerTimer.m_duration")}

	CTerrorPlayer["SetStaggerStart"] <- function(value){SetProp("m_staggerStart", value)}
	CTerrorPlayer["GetStaggerStart"] <- function(){return GetProp("m_staggerStart")}

	CTerrorPlayer["SetStaggerDir"] <- function(value){SetProp("m_staggerDir", value)}
	CTerrorPlayer["GetStaggerDir"] <- function(){return GetProp("m_staggerDir")}

	CTerrorPlayer["SetStaggerDist"] <- function(value){SetProp("m_staggerDist", value.tofloat())}
	CTerrorPlayer["GetStaggerDist"] <- function(){return GetProp("m_staggerDist")}

	CTerrorPlayer["SetTugTimestamp"] <- function(value){SetProp("m_tugTimer.m_timestamp", value.tofloat())}
	CTerrorPlayer["GetTugTimestamp"] <- function(){return GetProp("m_tugTimer.m_timestamp")}

	CTerrorPlayer["SetTugDuration"] <- function(value){SetProp("m_tugTimer.m_duration", value.tofloat())}
	CTerrorPlayer["GetTugDuration"] <- function(){return GetProp("m_tugTimer.m_duration")}

	CTerrorPlayer["SetTugStart"] <- function(value){SetProp("m_tugStart", value)}
	CTerrorPlayer["GetTugStart"] <- function(){return GetProp("m_tugStart")}

	CTerrorPlayer["SetTugDir"] <- function(value){SetProp("m_tugDir", value)}
	CTerrorPlayer["GetTugDir"] <- function(){return GetProp("m_tugDir")}

	CTerrorPlayer["SetTugDist"] <- function(value){SetProp("m_tugDist", value.tofloat())}
	CTerrorPlayer["GetTugDist"] <- function(){return GetProp("m_tugDist")}

	CTerrorPlayer["SetVocalizationSubject"] <- function(value){SetPropEntity("m_vocalizationSubject", value)}
	CTerrorPlayer["GetVocalizationSubject"] <- function(){return GetPropEntity("m_vocalizationSubject")}

	CTerrorPlayer["SetVocalizationSubjectTimestamp"] <- function(value){SetProp("m_vocalizationSubjectTimer.m_timestamp", value.tofloat())}
	CTerrorPlayer["GetVocalizationSubjectTimestamp"] <- function(){return GetProp("m_vocalizationSubjectTimer.m_timestamp")}

	CTerrorPlayer["SetVocalizationSubjectDuration"] <- function(value){SetProp("m_vocalizationSubjectTimer.m_duration", value.tofloat())}
	CTerrorPlayer["GetVocalizationSubjectDuration"] <- function(){return GetProp("m_vocalizationSubjectTimer.m_duration")}

	CTerrorPlayer["SetPounceStartPosition"] <- function(value){SetProp("m_pounceStartPosition", value)}
	CTerrorPlayer["GetPounceStartPosition"] <- function(){return GetProp("m_pounceStartPosition")}

	CTerrorPlayer["SetAttemptingToPounce"] <- function(value){SetProp("m_isAttemptingToPounce", value.tointeger())}
	CTerrorPlayer["GetAttemptingToPounce"] <- function(){return GetProp("m_isAttemptingToPounce")}

	CTerrorPlayer["SetVomitStart"] <- function(value){SetProp("m_vomitStart", value.tofloat())}
	CTerrorPlayer["GetVomitStart"] <- function(){return GetProp("m_vomitStart")}

	CTerrorPlayer["SetVomitFadeStart"] <- function(value){SetProp("m_vomitFadeStart", value.tofloat())}
	CTerrorPlayer["GetVomitFadeStart"] <- function(){return GetProp("m_vomitFadeStart")}

	CTerrorPlayer["SetBashedStart"] <- function(value){SetProp("m_bashedStart", value.tofloat())}
	CTerrorPlayer["GetBashedStart"] <- function(){return GetProp("m_bashedStart")}

	CTerrorPlayer["SetSalivaStart"] <- function(value){SetProp("m_salivaStart", value.tofloat())}
	CTerrorPlayer["GetSalivaStart"] <- function(){return GetProp("m_salivaStart")}

	CTerrorPlayer["SetVersusTeam"] <- function(value){SetProp("m_iVersusTeam", value.tointeger())}
	CTerrorPlayer["GetVersusTeam"] <- function(){return GetProp("m_iVersusTeam")}

	CTerrorPlayer["SetLagCompensation"] <- function(value){SetProp("m_bLagCompensation", value.tointeger())}
	CTerrorPlayer["GetLagCompensation"] <- function(){return GetProp("m_bLagCompensation")}

	CTerrorPlayer["SetPredictWeapons"] <- function(value){SetProp("m_bPredictWeapons", value.tointeger())}
	CTerrorPlayer["GetPredictWeapons"] <- function(){return GetProp("m_bPredictWeapons")}

	CTerrorPlayer["SetLastDamageAmount"] <- function(value){SetProp("m_lastDamageAmount", value.tointeger())}
	CTerrorPlayer["GetLastDamageAmount"] <- function(){return GetProp("m_lastDamageAmount")}

	CTerrorPlayer["SetTimeLastHurt"] <- function(value){SetProp("m_fTimeLastHurt", value.tofloat())}
	CTerrorPlayer["GetTimeLastHurt"] <- function(){return GetProp("m_fTimeLastHurt")}

	CTerrorPlayer["SetNextDecalTime"] <- function(value){SetProp("m_flNextDecalTime", value.tofloat())}
	CTerrorPlayer["GetNextDecalTime"] <- function(){return GetProp("m_flNextDecalTime")}

	CTerrorPlayer["SetImpulse"] <- function(value){SetProp("m_nImpulse", value.tointeger())}
	CTerrorPlayer["GetImpulse"] <- function(){return GetProp("m_nImpulse")}

	CTerrorPlayer["SetLastPlayerTalkTime"] <- function(value){SetProp("m_fLastPlayerTalkTime", value.tofloat())}
	CTerrorPlayer["GetLastPlayerTalkTime"] <- function(){return GetProp("m_fLastPlayerTalkTime")}

	CTerrorPlayer["SetUnderwater"] <- function(value){SetProp("m_bPlayerUnderwater", value.tointeger())}
	CTerrorPlayer["GetUnderwater"] <- function(){return GetProp("m_bPlayerUnderwater")}

	CTerrorPlayer["SetSinglePlayerGameEnding"] <- function(value){SetProp("m_bSinglePlayerGameEnding", value.tointeger())}
	CTerrorPlayer["GetSinglePlayerGameEnding"] <- function(){return GetProp("m_bSinglePlayerGameEnding")}

	CTerrorPlayer["SetAutoKickDisabled"] <- function(value){SetProp("m_autoKickDisabled", value.tointeger())}
	CTerrorPlayer["GetAutoKickDisabled"] <- function(){return GetProp("m_autoKickDisabled")}

	CTerrorPlayer["SetNumCrouches"] <- function(value){SetProp("m_nNumCrouches", value.tointeger())}
	CTerrorPlayer["GetNumCrouches"] <- function(){return GetProp("m_nNumCrouches")}

	CTerrorPlayer["SetDuckToggled"] <- function(value){SetProp("m_bDuckToggled", value.tointeger())}
	CTerrorPlayer["GetDuckToggled"] <- function(){return GetProp("m_bDuckToggled")}

	CTerrorPlayer["SetForwardMove"] <- function(value){SetProp("m_flForwardMove", value.tofloat())}
	CTerrorPlayer["GetForwardMove"] <- function(){return GetProp("m_flForwardMove")}

	CTerrorPlayer["SetSideMove"] <- function(value){SetProp("m_flSideMove", value.tofloat())}
	CTerrorPlayer["GetSideMove"] <- function(){return GetProp("m_flSideMove")}

	CTerrorPlayer["SetLastHitGroup"] <- function(value){SetProp("m_LastHitGroup", value.tointeger())}
	CTerrorPlayer["GetLastHitGroup"] <- function(){return GetProp("m_LastHitGroup")}

	CTerrorPlayer["SetForceServerRagdoll"] <- function(value){SetProp("m_bForceServerRagdoll", value.tointeger())}
	CTerrorPlayer["GetForceServerRagdoll"] <- function(){return GetProp("m_bForceServerRagdoll")}

	CTerrorPlayer["SetPreventWeaponPickup"] <- function(value){SetProp("m_bPreventWeaponPickup", value.tointeger())}
	CTerrorPlayer["GetPreventWeaponPickup"] <- function(){return GetProp("m_bPreventWeaponPickup")}

	CTerrorPlayer["SetRenderFX"] <- function(value){SetProp("m_nRenderFX", value.tointeger())}
	CTerrorPlayer["GetRenderFX"] <- function(){return GetProp("m_nRenderFX")}

	CTerrorPlayer["SetRenderMode"] <- function(value){SetProp("m_nRenderMode", value.tointeger())}
	CTerrorPlayer["GetRenderMode"] <- function(){return GetProp("m_nRenderMode")}

	CTerrorPlayer["SetFOVRate"] <- function(value){SetProp("m_Local.m_flFOVRate", value.tofloat())}
	CTerrorPlayer["GetFOVRate"] <- function(){return GetProp("m_Local.m_flFOVRate")}

	CTerrorPlayer["SetPostProcessController"] <- function(value){SetPropEntity("m_hPostProcessCtrl", value)}
	CTerrorPlayer["GetPostProcessController"] <- function(){return GetPropEntity("m_hPostProcessCtrl")}

	CTerrorPlayer["SetColorCorrectionController"] <- function(value){SetPropEntity("m_hColorCorrectionCtrl", value)}
	CTerrorPlayer["GetColorCorrectionController"] <- function(){return GetPropEntity("m_hColorCorrectionCtrl")}

	CTerrorPlayer["SetFogController"] <- function(value){SetPropEntity("m_PlayerFog.m_hCtrl", value)}
	CTerrorPlayer["GetFogController"] <- function(){return GetPropEntity("m_PlayerFog.m_hCtrl")}

	CTerrorPlayer["SetWeapon"] <- function(value, index){SetPropEntityArray("m_hMyWeapons", value, index.tointeger())}
	CTerrorPlayer["GetWeapon"] <- function(index){return GetPropEntityArray("m_hMyWeapons", index.tointeger())}

	CTerrorPlayer["SetLadderNormal"] <- function(value){SetProp("m_vecLadderNormal", value)}
	CTerrorPlayer["GetLadderNormal"] <- function(){return GetProp("m_vecLadderNormal")}

	CTerrorPlayer["SetLastLadderNormal"] <- function(value){SetProp("m_lastLadderNormal", value)}
	CTerrorPlayer["GetLastLadderNormal"] <- function(){return GetProp("m_lastLadderNormal")}

	CTerrorPlayer["IgnoreFallDamage"] <- function(){Input("IgnoreFallDamage")}
	CTerrorPlayer["IgnoreFallDamageWithoutReset"] <- function(){Input("IgnoreFallDamageWithoutReset")}

	CTerrorPlayer["EnableLedgeHang"] <- function(){Input("EnableLedgeHang")}
	CTerrorPlayer["DisableLedgeHang"] <- function(){Input("DisableLedgeHang")}

	CTerrorPlayer["SpeakResponseConcept"] <- function(){Input("SpeakResponseConcept")}

	CTerrorPlayer["TeleportToSurvivorPosition"] <- function(){Input("TeleportToSurvivorPosition")}

	CTerrorPlayer["ReleaseFromSurvivorPosition"] <- function(){Input("ReleaseFromSurvivorPosition")}

	CTerrorPlayer["SetGlowEnabled"] <- function(){Input("SetGlowEnabled")}

	CTerrorPlayer["RemoveWeaponUpgrades"] <- function(){Input("RemoveWeaponUpgrades")}

	CTerrorPlayer["CancelCurrentScene"] <- function(){Input("CancelCurrentScene")}

	CTerrorPlayer["CommandBot"] <- function(commandTable){
		commandTable["bot"] <- this
		CommandABot(commandTable)
	}

	CTerrorPlayer["GetInterp"] <- function(){return GetPropFloat("m_fLerpTime")}

	CTerrorPlayer["GetUpdateRate"] <- function(){return GetPropInt("m_nUpdateRate")}

	CTerrorPlayer["SetModelScale"] <- function(modelScale){SetPropFloat("m_flModelScale", modelScale.tofloat())}
	CTerrorPlayer["GetModelScale"] <- function(){return GetPropFloat("m_flModelScale")}

	CTerrorPlayer["SetThirdperson"] <- function(thirdperson){SetPropFloat("m_TimeForceExternalView", thirdperson ? 2147483647 : 0)}
	CTerrorPlayer["IsInThirdperson"] <- function(){return Time() < GetPropFloat("m_TimeForceExternalView")}

	CTerrorPlayer["GetTongueVictim"] <- function(){return GetPropEntity("m_tongueVictim")}
	CTerrorPlayer["GetTongueAttacker"] <- function(){return GetPropEntity("m_tongueOwner")}
	CTerrorPlayer["GetPounceVictim"] <- function(){return GetPropEntity("m_pounceVictim")}
	CTerrorPlayer["GetPounceAttacker"] <- function(){return GetPropEntity("m_pounceAttacker")}
	CTerrorPlayer["GetLeapVictim"] <- function(){return GetPropEntity("m_jockeyVictim")}
	CTerrorPlayer["GetLeapAttacker"] <- function(){return GetPropEntity("m_jockeyAttacker")}
	CTerrorPlayer["GetChargeVictim"] <- function(){return GetPropEntity("m_pummelVictim")}
	CTerrorPlayer["GetChargeAttacker"] <- function(){return GetPropEntity("m_pummelAttacker")}
	CTerrorPlayer["GetCarryVictim"] <- function(){return GetPropEntity("m_carryVictim")}
	CTerrorPlayer["GetCarryAttacker"] <- function(){return GetPropEntity("m_carryAttacker")}

	CTerrorPlayer["AddDisabledButton"] <- function(disabledButton){SetPropInt("m_afButtonDisabled", GetPropInt("m_afButtonDisabled") | disabledButton.tointeger())}
	CTerrorPlayer["RemoveDisabledButton"] <- function(disabledButton){SetPropInt("m_afButtonDisabled", GetPropInt("m_afButtonDisabled") & ~disabledButton.tointeger())}
	CTerrorPlayer["SetDisabledButtons"] <- function(disabledButtons){SetPropInt("m_afButtonDisabled", disabledButtons.tointeger())}
	CTerrorPlayer["GetDisabledButtons"] <- function(){return GetPropInt("m_afButtonDisabled")}
	CTerrorPlayer["HasDisabledButton"] <- function(disabledButton){return GetDisabledButtons() & disabledButton}

	CTerrorPlayer["AddForcedButton"] <- function(forcedButton){SetPropInt("m_afButtonForced", GetPropInt("m_afButtonForced") | forcedButton.tointeger())}
	CTerrorPlayer["RemoveForcedButton"] <- function(forcedButton){SetPropInt("m_afButtonForced", GetPropInt("m_afButtonForced") & ~forcedButton.tointeger())}
	CTerrorPlayer["SetForcedButtons"] <- function(forcedButtons){SetPropInt("m_afButtonForced", forcedButtons.tointeger())}
	CTerrorPlayer["GetForcedButtons"] <- function(){return GetPropInt("m_afButtonForced")}
	CTerrorPlayer["HasForcedButton"] <- function(forcedButton){return GetForcedButtons() & forcedButton}

	CTerrorPlayer["SetPresentAtSurvivalStart"] <- function(presentAtSurvivalStart){SetPropInt("m_bWasPresentAtSurvivalStart", presentAtSurvivalStart.tointeger())}
	CTerrorPlayer["WasPresentAtSurvivalStart"] <- function(){return GetPropInt("m_bWasPresentAtSurvivalStart")}

	CTerrorPlayer["SetGhost"] <- function(ghost){SetPropInt("m_isGhost", ghost.tointeger())}
	CTerrorPlayer["GetGhost"] <- function(){return GetProp("m_isGhost")}

	CTerrorPlayer["SetUsingMountedGun"] <- function(usingMountedGun){SetPropInt("m_usingMountedGun", usingMountedGun.tointeger())}
	CTerrorPlayer["IsUsingMountedGun"] <- function(){return GetPropInt("m_usingMountedGun")}

	CTerrorPlayer["SetUsingMountedWeapon"] <- function(value){SetProp("m_usingMountedWeapon", value.tointeger())}
	CTerrorPlayer["GetUsingMountedWeapon"] <- function(){return GetProp("m_usingMountedWeapon")}

	CTerrorPlayer["SetFirstManOut"] <- function(value){SetProp("m_bIsFirstManOut", value.tointeger())}
	CTerrorPlayer["GetFirstManOut"] <- function(){return GetPropInt("m_bIsFirstManOut")}

	CTerrorPlayer["SetOnThirdStrike"] <- function(value){SetProp("m_bIsOnThirdStrike", value.tointeger())}
	CTerrorPlayer["GetOnThirdStrike"] <- function(){return GetProp("m_bIsOnThirdStrike")}

	CTerrorPlayer["SetProneTongueDrag"] <- function(value){SetProp("m_isProneTongueDrag", value.tointeger())}
	CTerrorPlayer["GetProneTongueDrag"] <- function(){return GetPropInt("m_isProneTongueDrag")}

	CTerrorPlayer["SetReachedTongueOwner"] <- function(value){SetProp("m_reachedTongueOwner", value.tointeger())}
	CTerrorPlayer["GetReachedTongueOwner"] <- function(){return GetPropInt("m_reachedTongueOwner")}

	CTerrorPlayer["SetHangingFromTongue"] <- function(value){SetProp("m_reachedTongueOwner", value.tointeger())}
	CTerrorPlayer["GetHangingFromTongue"] <- function(){return GetPropInt("m_isHangingFromTongue")}

	CTerrorPlayer["SetReviveTarget"] <- function(reviveTarget){SetPropEntity("m_reviveTarget", reviveTarget)}
	CTerrorPlayer["GetReviveTarget"] <- function(){return GetPropEntity("m_reviveTarget")}

	CTerrorPlayer["SetReviveOwner"] <- function(reviveOwner){SetPropEntity("m_reviveOwner", reviveOwner)}
	CTerrorPlayer["GetReviveOwner"] <- function(){return GetPropEntity("m_reviveOwner")}

	CTerrorPlayer["SetCurrentUseAction"] <- function(currentUseAction){SetPropInt("m_iCurrentUseAction", currentUseAction.tointeger())}
	CTerrorPlayer["GetCurrentUseAction"] <- function(){return GetPropInt("m_iCurrentUseAction")}

	CTerrorPlayer["SetUseActionTarget"] <- function(useActionTarget){SetPropEntity("m_useActionTarget", useActionTarget)}
	CTerrorPlayer["GetUseActionTarget"] <- function(){return GetPropEntity("m_useActionTarget")}

	CTerrorPlayer["SetUseActionOwner"] <- function(useActionOwner){SetPropEntity("m_useActionOwner", useActionOwner)}
	CTerrorPlayer["GetUseActionOwner"] <- function(){return GetPropEntity("m_useActionOwner")}

	CTerrorPlayer["SetNightvisionEnabled"] <- function(nightvisionEnabled){SetPropInt("m_bNightVisionOn", nightvisionEnabled.tointeger())}
	CTerrorPlayer["IsNightvisionEnabled"] <- function(){return GetPropInt("m_bNightVisionOn")}

	CTerrorPlayer["SetTimescale"] <- function(timescale){SetPropFloat("m_flLaggedMovementValue", timescale.tofloat())}
	CTerrorPlayer["GetTimescale"] <- function(){return GetPropFloat("m_flLaggedMovementValue")}

	CTerrorPlayer["SetDrawViewmodel"] <- function(drawViewmodel){SetPropInt("m_bDrawViewmodel", drawViewmodel.tointeger())}
	CTerrorPlayer["GetDrawViewmodel"] <- function(){return GetPropInt("m_bDrawViewmodel")}

	CTerrorPlayer["SetFallVelocity"] <- function(fallVelocity){SetPropFloat("m_flFallVelocity", fallVelocity)}
	CTerrorPlayer["GetFallVelocity"] <- function(){return GetPropFloat("m_flFallVelocity")}

	CTerrorPlayer["SetHideHUD"] <- function(hideHUD){SetPropInt("m_Local.m_iHideHUD", hideHUD.tointeger())}
	CTerrorPlayer["AddHideHUD"] <- function(value){SetProp("m_Local.m_iHideHUD", GetProp("m_Local.m_iHideHUD") | value.tointeger())}
	CTerrorPlayer["RemoveHideHUD"] <- function(value){SetProp("m_iHideHUD", GetProp("m_Local.m_iHideHUD") & ~value.tointeger())}
	CTerrorPlayer["GetHideHUD"] <- function(){return GetPropInt("m_Local.m_iHideHUD")}
	CTerrorPlayer["HasHideHUD"] <- function(value){return GetProp("m_Local.m_iHideHUD") & value.tointeger()}

	CTerrorPlayer["SetViewmodel"] <- function(viewmodel){SetPropEntity("m_hViewModel", viewmodel)}
	CTerrorPlayer["GetViewmodel"] <- function(){return GetPropEntity("m_hViewModel")}

	CTerrorPlayer["SetZoom"] <- function(zoom){SetPropInt("m_iFOV", zoom.tointeger())}
	CTerrorPlayer["GetZoom"] <- function(){return GetPropInt("m_iFOV")}

	CTerrorPlayer["SetForcedObserverMode"] <- function(forcedObserverMode){SetPropInt("m_bForcedObserverMode", forcedObserverMode.tointeger())}
	CTerrorPlayer["GetForcedObserverMode"] <- function(){return GetPropInt("m_bForcedObserverMode")}

	CTerrorPlayer["SetObserverTarget"] <- function(observerTarget){SetPropEntity("m_hObserverTarget", observerTarget)}
	CTerrorPlayer["GetObserverTarget"] <- function(){return GetPropEntity("m_hObserverTarget")}

	CTerrorPlayer["SetObserverLastMode"] <- function(observerLastMode){SetPropInt("m_iObserverLastMode", observerLastMode.tointeger())}
	CTerrorPlayer["GetObserverLastMode"] <- function(){return GetPropInt("m_iObserverLastMode")}

	CTerrorPlayer["SetObserverMode"] <- function(observerMode){SetPropInt("m_iObserverMode", observerMode.tointeger())}
	CTerrorPlayer["GetObserverMode"] <- function(){return GetPropInt("m_iObserverMode")}

	CTerrorPlayer["SetSurvivorCharacter"] <- function(survivorCharacter){SetPropInt("m_survivorCharacter", survivorCharacter.tointeger())}
	CTerrorPlayer["GetSurvivorCharacter"] <- function(){return GetPropInt("m_survivorCharacter")}

	CTerrorPlayer["SetCalm"] <- function(value){SetProp("m_isCalm", value.tointeger())}
	CTerrorPlayer["GetCalm"] <- function(){return GetPropInt("m_isCalm")}

	CTerrorPlayer["SetCustomAbility"] <- function(customAbility){SetPropEntity("m_customAbility", customAbility)}
	CTerrorPlayer["GetCustomAbility"] <- function(){return GetPropEntity("m_customAbility")}

	CTerrorPlayer["SetSurvivorGlowEnabled"] <- function(survivorGlowEnabled){SetPropInt("m_bSurvivorGlowEnabled", survivorGlowEnabled.tointeger())}
	CTerrorPlayer["GetSurvivorGlowEnabled"] <- function(){return GetProp("m_bSurvivorGlowEnabled")}

	CTerrorPlayer["SetIntensity"] <- function(value){SetProp("m_clientIntensity", value.tointeger())}
	CTerrorPlayer["GetIntensity"] <- function(){return GetPropInt("m_clientIntensity")}

	CTerrorPlayer["SetFallingFromLedge"] <- function(value){SetProp("m_isFallingFromLedge", value.tointeger())}
	CTerrorPlayer["GetFallingFromLedge"] <- function(){return GetPropInt("m_isFallingFromLedge")}

	CTerrorPlayer["ClearJumpSuppression"] <- function(){SetPropFloat("m_jumpSupressedUntil", 0)}
	CTerrorPlayer["SuppressJump"] <- function(time){SetPropFloat("m_jumpSupressedUntil", Time() + time.tofloat())}

	CTerrorPlayer["SetMaxHealth"] <- function(maxHealth){SetPropInt("m_iMaxHealth", maxHealth.tointeger())}
	CTerrorPlayer["GetMaxHealth"] <- function(){return GetProp("m_iMaxHealth")}

	CTerrorPlayer["SetAirMovementRestricted"] <- function(airMovementRestricted){SetPropInt("m_airMovementRestricted", airMovementRestricted.tointeger())}
	CTerrorPlayer["GetAirMovementRestricted"] <- function(){return GetPropInt("m_airMovementRestricted")}

	CTerrorPlayer["GetFlowDistance"] <- function(){return GetCurrentFlowDistanceForPlayer(this)}
	CTerrorPlayer["GetFlowPercent"] <- function(){return GetCurrentFlowPercentForPlayer(this)}

	CTerrorPlayer["GetCharacterName"] <- function(){return GetCharacterDisplayName(this)}

	CTerrorPlayer["Say"] <- function(message, teamOnly = false){::Say(this, message, teamOnly)}

	CTerrorPlayer["IsBot"] <- function(){return IsPlayerABot(this)}

	CTerrorPlayer["PickupObject"] <- function(entity){PickupObject(this, entity)}

	CTerrorPlayer["SetEyeAngles"] <- function(angles){
		local prevPlayerName = GetName()
		local playerName = UniqueString()
		SetName(playerName)
		local teleportEntity = SpawnEntityFromTable("point_teleport", {origin = GetOrigin(), angles = angles.ToKVString(), target = playerName, targetname = UniqueString()})
		DoEntFire("!self", "Teleport", "", 0, null, teleportEntity)
		DoEntFire("!self", "Kill", "", 0, null, teleportEntity)
		DoEntFire("!self", "AddOutput", "targetname " + prevPlayerName, 0.01, null, this)
	}

	CTerrorPlayer["GetLifeState"] <- function(){return GetPropInt("m_lifeState")}

	CTerrorPlayer["PlaySoundOnClient"] <- function(soundName){EmitSoundOnClient(soundName, this)}

	CTerrorPlayer["GetAmmo"] <- function(weapon){return GetPropIntArray("m_iAmmo", weapon.GetPropInt("m_iPrimaryAmmoType"))}
	CTerrorPlayer["SetAmmo"] <- function(weapon, ammo){SetPropIntArray("m_iAmmo", weapon.GetPropInt("m_iPrimaryAmmoType"), ammo)}
	
	
	::MoreMethods_Finished <- true
}

::MoreMethods_Check <- function(){
	if(::MoreMethods_Finished){
		return
	}
	local _player = null
	while(_player = Entities.FindByClassname(_player, "player")){break}
	if(("CBaseEntity" in getroottable()) && ("CBaseAnimating" in getroottable()) && ("CTerrorPlayer" in getroottable())){
		MoreMethods_Include()
	} else {
		EntFire("worldspawn", "RunScriptCode", "::MoreMethods_Check()", 0.001)
	}
}

MoreMethods_Check()
