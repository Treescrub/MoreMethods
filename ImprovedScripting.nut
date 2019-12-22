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
 
CBaseEntity["HasProp"] <- function(propertyName){return NetProps.HasProp(this, propertyName)}
CBaseEntity["GetPropType"] <- function(propertyName){return NetProps.GetPropType(this, propertyName)}
CBaseEntity["GetPropArraySize"] <- function(propertyName){return NetProps.GetPropArraySize(this, propertyName)}

CBaseEntity["GetPropInt"] <- function(propertyName){return NetProps.GetPropInt(this, propertyName)}
CBaseEntity["GetPropEntity"] <- function(propertyName){return NetProps.GetPropEntity(this, propertyName)}
CBaseEntity["GetPropString"] <- function(propertyName){return NetProps.GetPropString(this, propertyName)}
CBaseEntity["GetPropFloat"] <- function(propertyName){return NetProps.GetPropFloat(this, propertyName)}
CBaseEntity["GetPropVector"] <- function(propertyName){return NetProps.GetPropVector(this, propertyName)}
CBaseEntity["SetPropInt"] <- function(propertyName, value){NetProps.SetPropInt(this, propertyName, value)}
CBaseEntity["SetPropEntity"] <- function(propertyName, value){NetProps.SetPropEntity(this, propertyName, value)}
CBaseEntity["SetPropString"] <- function(propertyName, value){NetProps.SetPropString(this, propertyName, value)}
CBaseEntity["SetPropFloat"] <- function(propertyName, value){NetProps.SetPropFloat(this, propertyName, value)}
CBaseEntity["SetPropVector"] <- function(propertyName, value){NetProps.SetPropVector(this, propertyName, value)}

CBaseEntity["GetPropIntArray"] <- function(propertyName, index){return NetProps.GetPropIntArray(this, propertyName, index)}
CBaseEntity["GetPropEntityArray"] <- function(propertyName, index){return NetProps.GetPropEntityArray(this, propertyName, index)}
CBaseEntity["GetPropStringArray"] <- function(propertyName, index){return NetProps.GetPropStringArray(this, propertyName, index)}
CBaseEntity["GetPropFloatArray"] <- function(propertyName, index){return NetProps.GetPropFloatArray(this, propertyName, index)}
CBaseEntity["GetPropVectorArray"] <- function(propertyName, index){return NetProps.GetPropVectorArray(this, propertyName, index)}
CBaseEntity["SetPropIntArray"] <- function(propertyName, index, value){NetProps.SetPropIntArray(this, propertyName, value, index)}
CBaseEntity["SetPropEntityArray"] <- function(propertyName, index, value){NetProps.SetPropEntityArray(this, propertyName, value, index)}
CBaseEntity["SetPropStringArray"] <- function(propertyName, index, value){NetProps.SetPropStringArray(this, propertyName, value, index)}
CBaseEntity["SetPropFloatArray"] <- function(propertyName, index, value){NetProps.SetPropFloatArray(this, propertyName, value, index)}
CBaseEntity["SetPropVectorArray"] <- function(propertyName, index, value){NetProps.SetPropVectorArray(this, propertyName, value, index)}

CBaseEntity["SetProp"] <- function(propertyName, value, index = null){
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
CBaseEntity["GetProp"] <- function(propertyName, index = null, asArray = false){
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

CBaseEntity["SetRenderFX"] <- function(value){SetProp("m_nRenderFX", value.tointeger())}
CBaseEntity["GetRenderFX"] <- function(){return GetProp("m_nRenderFX")}

CBaseEntity["SetRenderMode"] <- function(value){SetProp("m_nRenderMode", value.tointeger())}
CBaseEntity["GetRenderMode"] <- function(){return GetProp("m_nRenderMode")}

CBaseEntity["GetModelIndex"] <- function(){return GetPropInt("m_nModelIndex")}
CBaseEntity["GetModelName"] <- function(){return GetPropString("m_ModelName")}

CBaseEntity["SetName"] <- function(name){SetPropString("m_iName", name)}

CBaseEntity["GetFriction"] <- function(){return GetFriction(this)}
CBaseEntity["GetPhysVelocity"] <- function(){return GetPhysVelocity(this)}

CBaseEntity["GetFlags"] <- function(){return GetPropInt("m_fFlags")}
CBaseEntity["SetFlags"] <- function(flag){SetPropInt("m_fFlags", flag)}
CBaseEntity["AddFlag"] <- function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") | flag)}
CBaseEntity["RemoveFlag"] <- function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") & ~flag)}
CBaseEntity["HasFlag"] <- function(flag){return GetFlags() & flag}

CBaseEntity["GetMoveType"] <- function(){return GetPropInt("movetype")}
CBaseEntity["SetMoveType"] <- function(type){SetPropInt("movetype", type)}

CBaseEntity["GetSpawnflags"] <- function(){return GetPropInt("m_spawnflags")}
CBaseEntity["SetSpawnFlags"] <- function(flags){SetPropInt("m_spawnflags", flags)}

CBaseEntity["GetGlowType"] <- function(){return GetPropInt("m_Glow.m_iGlowType")}
CBaseEntity["SetGlowType"] <- function(type){SetPropInt("m_Glow.m_iGlowType", type)}

CBaseEntity["GetGlowRange"] <- function(){return GetPropInt("m_Glow.m_nGlowRange")}
CBaseEntity["SetGlowRange"] <- function(range){SetPropInt("m_Glow.m_nGlowRange", range)}

CBaseEntity["GetGlowRangeMin"] <- function(){return GetPropInt("m_Glow.m_nGlowRangeMin")}
CBaseEntity["SetGlowRangeMin"] <- function(range){SetPropInt("m_Glow.m_nGlowRangeMin", range)}

CBaseEntity["GetGlowColor"] <- function(){return GetPropInt("m_Glow.m_glowColorOverride")}
CBaseEntity["SetGlowColor"] <- function(r, g, b){
	local color = r
	color += 256 * g
	color += 65536 * b
	SetPropInt("m_Glow.m_glowColorOverride", color)
}
CBaseEntity["SetGlowColorVector"] <- function(vector){
	local color = vector.x
	color += 256 * vector.y
	color += 65536 * vector.z
	SetPropInt("m_Glow.m_glowColorOverride", color)
}
CBaseEntity["ResetGlowColor"] <- function(){SetPropInt("m_Glow.m_glowColorOverride", -1)}

CBaseEntity["SetTeam"] <- function(team){SetPropInt("m_iTeamNum", team.tointeger())}
CBaseEntity["GetTeam"] <- function(){return GetPropInt("m_iTeamNum")}

CBaseEntity["GetGlowFlashing"] <- function(){return GetPropInt("m_Glow.m_bFlashing")}
CBaseEntity["SetGlowFlashing"] <- function(flashing){SetPropInt("m_Glow.m_bFlashing", flashing)}

CBaseEntity["GetFlowDistance"] <- function(){return GetFlowDistanceForPosition(GetOrigin())}
CBaseEntity["GetFlowPercent"] <- function(){return GetFlowPercentForPosition(GetOrigin())}

CBaseEntity["PlaySound"] <- function(soundName){EmitSoundOn(soundName, this)}
CBaseEntity["StopSound"] <- function(soundName){StopSoundOn(soundName, this)}

CBaseEntity["Input"] <- function(input, value = "", delay = 0, activator = null){DoEntFire("!self", input.tostring(), value.tostring(), delay.tofloat(), activator, this)}
CBaseEntity["SetAlpha"] <- function(alpha){Input("Alpha", alpha)}
CBaseEntity["Enable"] <- function(){Input("Enable")}
CBaseEntity["Disable"] <- function(){Input("Disable")}
CBaseEntity["GetValidatedScriptScope"] <- function(){
	ValidateScriptScope()
	return GetScriptScope()
}


// Beginning of entity specific NetProps
CBaseEntity["SetStartScale"] <- function(startScale){SetProp("m_flStartScale", startScale.tofloat())}
CBaseEntity["GetStartScale"] <- function(){return GetProp("m_flStartScale")}
	
CBaseEntity["SetScale"] <- function(scale){SetProp("m_flScale", scale.tofloat())}
CBaseEntity["GetScale"] <- function(){return GetProp("m_flScale")}
	
CBaseEntity["SetScaleTime"] <- function(scaleTime){SetProp("m_flScaleTime", scaleTime.tofloat())}
CBaseEntity["GetScaleTime"] <- function(){return GetProp("m_flScaleTime")}
	
CBaseEntity["SetFlags"] <- function(flags){SetProp("m_nFlags", flags.tointeger())}
CBaseEntity["GetFlags"] <- function(){return GetProp("m_nFlags")}
	
CBaseEntity["SetFlameModelIndex"] <- function(flameModelIndex){SetProp("m_nFlameModelIndex", flameModelIndex.tointeger())}
CBaseEntity["GetFlameModelIndex"] <- function(){return GetProp("m_nFlameModelIndex")}
	
CBaseEntity["SetFlameFromAboveModelIndex"] <- function(flameFromAboveModelIndex){SetProp("m_nFlameFromAboveModelIndex", flameFromAboveModelIndex.tointeger())}
CBaseEntity["GetFlameFromAboveModelIndex"] <- function(){return GetProp("m_nFlameFromAboveModelIndex")}


CBaseEntity["SetStartScale"] <- function(startScale){SetProp("m_flStartScale", startScale.tofloat())}
CBaseEntity["GetStartScale"] <- function(){return GetProp("m_flStartScale")}
	
CBaseEntity["SetScale"] <- function(scale){SetProp("m_flScale", scale.tofloat())}
CBaseEntity["GetScale"] <- function(){return GetProp("m_flScale")}
	
CBaseEntity["SetScaleTime"] <- function(scaleTime){SetProp("m_flScaleTime", scaleTime.tofloat())}
CBaseEntity["GetScaleTime"] <- function(){return GetProp("m_flScaleTime")}
	
CBaseEntity["SetPlasmaModelIndex"] <- function(plasmaModelIndex){SetProp("m_nPlasmaModelIndex", plasmaModelIndex.tointeger())}
CBaseEntity["GetPlasmaModelIndex"] <- function(){return GetProp("m_nPlasmaModelIndex")}
	
CBaseEntity["SetPlasmaModelIndex2"] <- function(plasmaModelIndex2){SetProp("m_nPlasmaModelIndex2", plasmaModelIndex2.tointeger())}
CBaseEntity["GetPlasmaModelIndex2"] <- function(){return GetProp("m_nPlasmaModelIndex2")}
	
CBaseEntity["SetGlowModelIndex"] <- function(glowModelIndex){SetProp("m_nGlowModelIndex", glowModelIndex.tointeger())}
CBaseEntity["GetGlowModelIndex"] <- function(){return GetProp("m_nGlowModelIndex")}


CBaseEntity["SetCharging"] <- function(charging){SetProp("m_isCharging", charging.tointeger())}
CBaseEntity["IsCharging"] <- function(){return GetProp("m_isCharging")}
	
CBaseEntity["SetOwner"] <- function(value){SetPropEntity("m_owner", value)}
CBaseEntity["GetOwner"] <- function(value){return GetPropEntity("m_owner")}
	
CBaseEntity["SetChargeStartTime"] <- function(chargeStartTime){SetProp("m_chargeStartTime", chargeStartTime.tofloat())}
CBaseEntity["GetChargeStartTime"] <- function(){return GetProp("m_chargeStartTime")}
	
CBaseEntity["SetPotentialTarget"] <- function(potentialTarget){SetPropEntity("m_hPotentialTarget", potentialTarget)}
CBaseEntity["GetPotentialTarget"] <- function(){return GetPropEntity("m_hPotentialTarget")}


CBaseEntity["SetLeaping"] <- function(leaping){SetProp("m_isCharging", leaping.tointeger())}
CBaseEntity["IsLeaping"] <- function(){return GetProp("m_isLeaping")}
	
CBaseEntity["SetOwner"] <- function(value){SetPropEntity("m_owner", value)}
CBaseEntity["GetOwner"] <- function(value){return GetPropEntity("m_owner")}
	
CBaseEntity["SetLeapStartTime"] <- function(leapStartTime){SetProp("m_leapStartTime", leapStartTime.tofloat())}
CBaseEntity["GetLeapStartTime"] <- function(){return GetProp("m_leapStartTime")}
	
CBaseEntity["SetQueuedLeap"] <- function(queuedLeap){SetProp("m_queuedLeap", queuedLeap)}
CBaseEntity["GetQueuedLeap"] <- function(){return GetProp("m_queuedLeap")}


CBaseEntity["SetLunging"] <- function(lunging){SetProp("m_isCharging", lunging.tointeger())}
CBaseEntity["IsLunging"] <- function(){return GetProp("m_isLunging")}
	
CBaseEntity["SetOwner"] <- function(value){SetPropEntity("m_owner", value)}
CBaseEntity["GetOwner"] <- function(value){return GetPropEntity("m_owner")}
	
CBaseEntity["SetLungeStartTime"] <- function(lungeStartTime){SetProp("m_lungeStartTime", lungeStartTime.tofloat())}
CBaseEntity["GetLungeStartTime"] <- function(){return GetProp("m_lungeStartTime")}
	
CBaseEntity["SetQueuedLeap"] <- function(queuedLeap){SetProp("m_queuedLeap", queuedLeap)}
CBaseEntity["GetQueuedLeap"] <- function(){return GetProp("m_queuedLeap")}


CBaseEntity["SetActivated"] <- function(hasBeenActivated){SetProp(m_bHasBeenActivated, hasBeenActivated.tointeger())}
CBaseEntity["HasBeenActivated"] <- function(){return GetProp("m_bHasBeenActivated")}
	
CBaseEntity["SetOwner"] <- function(value){SetPropEntity("m_owner", value)}
CBaseEntity["GetOwner"] <- function(value){return GetPropEntity("m_owner")}


CBaseEntity["SetActivated"] <- function(hasBeenActivated){SetProp(m_bHasBeenActivated, hasBeenActivated.tointeger())}
CBaseEntity["HasBeenActivated"] <- function(){return GetProp("m_bHasBeenActivated")}
	
CBaseEntity["SetOwner"] <- function(value){SetPropEntity("m_owner", value)}
CBaseEntity["GetOwner"] <- function(value){return GetPropEntity("m_owner")}


CBaseEntity["SetTongueGrabStartingHealth"] <- function(tongueGrabStartingHealth){SetProp("m_tongueGrabStartingHealth", tongueGrabStartingHealth.tointeger())}
CBaseEntity["GetTongueGrabStartingHealth"] <- function(){return GetProp("m_tongueGrabStartingHealth")}
	
CBaseEntity["SetOwner"] <- function(value){SetPropEntity("m_owner", value)}
CBaseEntity["GetOwner"] <- function(value){return GetPropEntity("m_owner")}
	
CBaseEntity["SetTongueHitRange"] <- function(tongueHitRange){SetProp("m_tongueHitRange", tongueHitRange.tofloat())}
CBaseEntity["GetTongueHitRange"] <- function(){return GetProp("m_tongueHitRange")}
	
CBaseEntity["SetTongueHitTimestamp"] <- function(tongueHitTimestamp){SetProp("m_tongueHitTimestamp", tongueHitTimestamp.tofloat())}
CBaseEntity["GetTongueHitTimestamp"] <- function(){return GetProp("m_tongueHitTimestamp")}
	
CBaseEntity["SetTongueHitWasAmbush"] <- function(tongueHitWasAmbush){SetProp("m_tongueHitWasAmbush", tongueHitWasAmbush.tointeger())}
CBaseEntity["GetTongueHitWasAmbush"] <- function(){return GetProp("m_tongueHitWasAmbush")}
	
CBaseEntity["SetTongueVictimDistance"] <- function(tongueVictimDistance){SetProp("m_tongueVictimDistance", tongueVictimDistance.tofloat())}
CBaseEntity["GetTongueVictimDistance"] <- function(){return GetProp("m_tongueVictimDistance")}
	
CBaseEntity["SetTongueVictimPositionTime"] <- function(tongueVictimPositionTime){SetProp("m_tongueVictimPositionTime", tongueVictimPositionTime.tofloat())}
CBaseEntity["GetTongueVictimPositionTime"] <- function(){return GetProp("m_tongueVictimPositionTime")}
	
CBaseEntity["SetTongueVictimLastOnGroundTime"] <- function(tongueVictimLastOnGroundTime){SetProp("m_tongueVictimLastOnGroundTime", tongueVictimLastOnGroundTime.tofloat())}
CBaseEntity["GetTongueVictimLastOnGroundTime"] <- function(){return GetProp("m_tongueVictimLastOnGroundTime")}
	
CBaseEntity["SetPotentialTarget"] <- function(potentialTarget){SetPropEntity("m_hPotentialTarget", potentialTarget)}
CBaseEntity["GetPotentialTarget"] <- function(){return GetPropEntity("m_hPotentialTarget")}
	
CBaseEntity["SetCurrentTipTarget"] <- function(currentTipTarget){SetPropEntity("m_currentTipTarget", currentTipTarget)}
CBaseEntity["GetCurrentTipTarget"] <- function(){return GetPropEntity("m_currentTipTarget")}
	
CBaseEntity["SetTongueState"] <- function(tongueState){SetProp("m_tongueState", tongueState.tointeger())}
CBaseEntity["GetTongueState"] <- function(){return GetProp("m_tongueState")}
	
CBaseEntity["SetBendPointCount"] <- function(bendPointCount){SetProp("m_bendPointCount", bendPointCount.tointeger())}
CBaseEntity["GetBendPointCount"] <- function(){return GetProp("m_bendPointCount")}
	
CBaseEntity["SetTipPosition"] <- function(tipPosition){SetProp("m_tipPosition", tipPosition)}
CBaseEntity["GetTipPosition"] <- function(){return GetProp("m_tipPosition")}
	
CBaseEntity["SetBendPositions"] <- function(bendPositions){SetProp("m_bendPositions", bendPositions)}
CBaseEntity["GetBendPositions"] <- function(){return GetProp("m_bendPositions", null, true)}
CBaseEntity["SetBendPosition"] <- function(bendPosition, index){SetProp("m_bendPositions", bendPosition, index)}
CBaseEntity["GetBendPosition"] <- function(index){return GetProp("m_bendPositions", index)}


CBaseEntity["SetSpraying"] <- function(spraying){SetProp("m_isSpraying", spraying.tointeger())}
CBaseEntity["IsSpraying"] <- function(){return GetProp("m_isSpraying")}
	
CBaseEntity["SetOwner"] <- function(value){SetPropEntity("m_owner", value)}
CBaseEntity["GetOwner"] <- function(value){return GetPropEntity("m_owner")}
	
CBaseEntity["SetAttackDuration"] <- function(attackDuration){SetProp("m_attackDuration.m_duration", attackDuration.tofloat())}
CBaseEntity["GetAttackDuration"] <- function(){return GetProp("m_attackDuration.m_duration")}
CBaseEntity["SetAttackDurationTimestamp"] <- function(attackDurationTimestamp){SetProp("m_attackDuration.m_timestamp", attackDurationTimestamp.tofloat())}
CBaseEntity["GetAttackDurationTimestamp"] <- function(){return GetProp("m_attackDuration.m_timestamp")}
	
CBaseEntity["SetNextSprayDuration"] <- function(nextSprayDuration){SetProp("m_nextSpray.m_duration", nextSprayDuration.tofloat())}
CBaseEntity["GetNextSprayDuration"] <- function(){return GetProp("m_nextSpray.m_duration")}
CBaseEntity["SetNextSprayTimestamp"] <- function(nextSprayTimestamp){SetProp("m_nextSpray.m_timestamp", nextSprayTimestamp.tofloat())}
CBaseEntity["GetNextSprayTimestamp"] <- function(){return GetProp("m_nextSpray.m_timestamp")}


CBaseEntity["SetFreezePeriod"] <- function(value){SetProp("terror_gamerules_data.m_bFreezePeriod", value.tointeger())}
CBaseEntity["GetFreezePeriod"] <- function(){return GetProp("terror_gamerules_data.m_bFreezePeriod")}

CBaseEntity["SetRoundTime"] <- function(value){SetProp("terror_gamerules_data.m_iRoundTime", value.tointeger())}
CBaseEntity["GetRoundTime"] <- function(){return GetProp("terror_gamerules_data.m_iRoundTime")}

CBaseEntity["SetLevelStartTime"] <- function(value){SetProp("terror_gamerules_data.m_fLevelStartTime", value.tofloat())}
CBaseEntity["GetLevelStartTime"] <- function(){return GetProp("terror_gamerules_data.m_fLevelStartTime")}

CBaseEntity["SetGameStartTime"] <- function(value){SetProp("terror_gamerules_data.m_flGameStartTime", value.tofloat())}
CBaseEntity["GetGameStartTime"] <- function(){return GetProp("terror_gamerules_data.m_flGameStartTime")}

CBaseEntity["SetInIntro"] <- function(value){SetProp("terror_gamerules_data.m_bInIntro", value.tointeger())}
CBaseEntity["GetInIntro"] <- function(){return GetProp("terror_gamerules_data.m_bInIntro")}

CBaseEntity["SetServerRank"] <- function(value){SetProp("terror_gamerules_data.m_iServerRank", value.tointeger())}
CBaseEntity["GetServerRank"] <- function(){return GetProp("terror_gamerules_data.m_iServerRank")}

CBaseEntity["SetServerPlayerCount"] <- function(value){SetProp("terror_gamerules_data.m_iServerPlayerCount", value.tointeger())}
CBaseEntity["GetServerPlayerCount"] <- function(){return GetProp("terror_gamerules_data.m_iServerPlayerCount")}

CBaseEntity["SetIsDedicatedServer"] <- function(value){SetProp("terror_gamerules_data.m_bIsDedicatedServer", value.tointeger())}
CBaseEntity["GetIsDedicatedServer"] <- function(){return GetProp("terror_gamerules_data.m_bIsDedicatedServer")}

CBaseEntity["SetServerSteamGroupID"] <- function(value){SetProp("terror_gamerules_data.m_iServerSteamGroupID", value.tointeger())}
CBaseEntity["GetServerSteamGroupID"] <- function(){return GetProp("terror_gamerules_data.m_iServerSteamGroupID")}

CBaseEntity["SetRoundStartTime"] <- function(value){SetProp("terror_gamerules_data.m_flRoundStartTime", value.tofloat())}
CBaseEntity["GetRoundStartTime"] <- function(){return GetProp("terror_gamerules_data.m_flRoundStartTime")}

CBaseEntity["SetRoundEndTime"] <- function(value){SetProp("terror_gamerules_data.m_flRoundEndTime", value.tofloat())}
CBaseEntity["GetRoundEndTime"] <- function(){return GetProp("terror_gamerules_data.m_flRoundEndTime")}

CBaseEntity["SetAccumulatedTime"] <- function(value){SetProp("terror_gamerules_data.m_flAccumulatedTime", value.tofloat())}
CBaseEntity["GetAccumulatedTime"] <- function(){return GetProp("terror_gamerules_data.m_flAccumulatedTime")}

CBaseEntity["SetRoundNumber"] <- function(value){SetProp("terror_gamerules_data.m_nRoundNumber", value.tointeger())}
CBaseEntity["GetRoundNumber"] <- function(){return GetProp("terror_gamerules_data.m_nRoundNumber")}

CBaseEntity["SetRoundLimit"] <- function(value){SetProp("terror_gamerules_data.m_nRoundLimit", value.tointeger())}
CBaseEntity["GetRoundLimit"] <- function(){return GetProp("terror_gamerules_data.m_nRoundLimit")}

CBaseEntity["SetTeamBestRoundTime"] <- function(value){SetProp("terror_gamerules_data.m_flTeamBestRoundTime", value.tofloat())}
CBaseEntity["GetTeamBestRoundTime"] <- function(){return GetProp("terror_gamerules_data.m_flTeamBestRoundTime")}

CBaseEntity["SetScavengeItemsRemaining"] <- function(value){SetProp("terror_gamerules_data.m_nScavengeItemsRemaining", value.tointeger())}
CBaseEntity["GetScavengeItemsRemaining"] <- function(){return GetProp("terror_gamerules_data.m_nScavengeItemsRemaining")}

CBaseEntity["SetScavengeItemsGoal"] <- function(value){SetProp("terror_gamerules_data.m_nScavengeItemsGoal", value.tointeger())}
CBaseEntity["GetScavengeItemsGoal"] <- function(){return GetProp("terror_gamerules_data.m_nScavengeItemsGoal")}

CBaseEntity["SetTeamsFlipped"] <- function(value){SetProp("terror_gamerules_data.m_bAreTeamsFlipped", value.tointeger())}
CBaseEntity["GetTeamsFlipped"] <- function(){return GetProp("terror_gamerules_data.m_bAreTeamsFlipped")}

CBaseEntity["SetSecondHalfOfRound"] <- function(value){SetProp("terror_gamerules_data.m_bInSecondHalfOfRound", value.tointeger())}
CBaseEntity["GetSecondHalfOfRound"] <- function(){return GetProp("terror_gamerules_data.m_bInSecondHalfOfRound")}

CBaseEntity["SetTransitioningToNextMap"] <- function(value){SetProp("terror_gamerules_data.m_bIsTransitioningToNextMap", value.tointeger())}
CBaseEntity["GetTransitioningToNextMap"] <- function(){return GetProp("terror_gamerules_data.m_bIsTransitioningToNextMap")}

CBaseEntity["SetVersusVoteRestarting"] <- function(value){SetProp("terror_gamerules_data.m_bIsVersusVoteRestarting", value.tointeger())}
CBaseEntity["GetVersusVoteRestarting"] <- function(){return GetProp("terror_gamerules_data.m_bIsVersusVoteRestarting")}

CBaseEntity["SetChallengeModeActive"] <- function(value){SetProp("terror_gamerules_data.m_bChallengeModeActive", value.tointeger())}
CBaseEntity["GetChallengeModeActive"] <- function(){return GetProp("terror_gamerules_data.m_bChallengeModeActive")}

CBaseEntity["SetSacrificeEscapees"] <- function(value){SetProp("terror_gamerules_data.m_iSacrificeEscapees", value.tointeger())}
CBaseEntity["GetSacrificeEscapees"] <- function(){return GetProp("terror_gamerules_data.m_iSacrificeEscapees")}

CBaseEntity["SetHoldoutCooldownEndTime"] <- function(value){SetProp("terror_gamerules_data.m_flHoldoutCooldownEndTime", value.tofloat())}
CBaseEntity["GetHoldoutCooldownEndTime"] <- function(){return GetProp("terror_gamerules_data.m_flHoldoutCooldownEndTime")}


CBaseEntity["GetListenServerHost"] <- function(){
		for(local i=0; i < GetPropArraySize("m_listenServerHost"); i++){
			if(GetProp("m_listenServerHost", i)){
				return EntIndexToHScript(i)
			}
		}
	}
	
CBaseEntity["SetTeamSwitchRule"] <- function(ent, value){
		if(ent.GetClassname() != "player" || !ent.IsValid()){
			return false
		}
		
		SetProp("m_TeamSwitchRule", value.tointeger(), ent.GetEntityIndex())
	}
CBaseEntity["GetTeamSwitchRule"] <- function(ent){
		if(ent.GetClassname() != "player" || !ent.IsValid()){
			return
		}
		
		return GetProp("m_TeamSwitchRule", ent.GetEntityIndex())
	}
	
CBaseEntity["GetSurvivalRecordTime"] <- function(ent){
		if(ent.GetClassname() != "player" || !ent.IsValid()){
			return
		}
		
		return GetProp("m_flSurvivalRecordTime", ent.GetEntityIndex())
	}
	
CBaseEntity["GetSurvivalTopMedal"] <- function(ent){
		if(ent.GetClassname() != "player" || !ent.IsValid()){
			return
		}
		
		return GetProp("m_nSurvivalTopMedal", ent.GetEntityIndex())
	}
	
CBaseEntity["SetBecomeGhostAt"] <- function(ent, value){
		if(ent.GetClassname() != "player" || !ent.IsValid()){
			return false
		}
		
		SetProp("m_flBecomeGhostAt", value.tofloat(), ent.GetEntityIndex())
	}
CBaseEntity["GetBecomeGhostAt"] <- function(ent){
		if(ent.GetClassname() != "player" || !ent.IsValid()){
			return
		}
		
		return GetProp("m_flBecomeGhostAt", ent.GetEntityIndex())
	}
	
CBaseEntity["SetTankLotteryEntryRatio"] <- function(value){SetProp("m_tankLotteryEntryRatio", value.tofloat())}
CBaseEntity["GetTankLotteryEntryRatio"] <- function(){return GetProp("m_tankLotteryEntryRatio")}

CBaseEntity["SetTankLotterySelectionRatio"] <- function(value){SetProp("m_tankLotterySelectionRatio", value.tofloat())}
CBaseEntity["GetTankLotterySelectionRatio"] <- function(){return GetProp("m_tankLotterySelectionRatio")}

CBaseEntity["SetPendingTankPlayerIndex"] <- function(value){SetProp("m_pendingTankPlayerIndex", value.tointeger())}
CBaseEntity["GetPendingTankPlayerIndex"] <- function(){return GetProp("m_pendingTankPlayerIndex")}

CBaseEntity["SetFinale"] <- function(value){SetProp("m_isFinale", value.tointeger())}
CBaseEntity["GetFinale"] <- function(){return GetProp("m_isFinale")}

CBaseEntity["SetIsSurvivorTeamReadyTime"] <- function(value){SetProp("m_isSurvivorTeamReadyTime", value.tointeger())}
CBaseEntity["GetIsSurvivorTeamReadyTime"] <- function(){return GetProp("m_isSurvivorTeamReadyTime")}

CBaseEntity["SetSurvivorTeamReadyTime"] <- function(value){SetProp("m_survivorTeamReadyTime", value.tointeger())}
CBaseEntity["GetSurvivorTeamReadyTime"] <- function(){return GetProp("m_survivorTeamReadyTime")}

CBaseEntity["SetRoundSetupTimeRemaining"] <- function(value){SetProp("m_nRoundSetupTimeRemaining", value.tointeger())}
CBaseEntity["GetRoundSetupTimeRemaining"] <- function(){return GetProp("m_nRoundSetupTimeRemaining")}

CBaseEntity["SetTempoState"] <- function(value){SetProp("m_tempoState", value.tointeger())}
CBaseEntity["GetTempoState"] <- function(){return GetProp("m_tempoState")}

CBaseEntity["SetAnySurvivorLeftSafeArea"] <- function(value){SetProp("m_hasAnySurvivorLeftSafeArea", value.tointeger())}
CBaseEntity["GetAnySurvivorLeftSafeArea"] <- function(){return GetProp("m_hasAnySurvivorLeftSafeArea")}

CBaseEntity["SetTeamFrozen"] <- function(value){SetProp("m_isTeamFrozen", value.tointeger())}
CBaseEntity["GetTeamFrozen"] <- function(){return GetProp("m_isTeamFrozen")}

CBaseEntity["SetMissionDuration"] <- function(value){SetProp("m_missionDuration", value.tointeger())}
CBaseEntity["GetMissionDuration"] <- function(){return GetProp("m_missionDuration")}

CBaseEntity["SetMissionWipes"] <- function(value){SetProp("m_missionWipes", value.tointeger())}
CBaseEntity["GetMissionWipes"] <- function(){return GetProp("m_missionWipes")}

CBaseEntity["SetSharedRandomSeed"] <- function(value){SetProp("m_sharedRandomSeed", value.tointeger())}
CBaseEntity["GetSharedRandomSeed"] <- function(){return GetProp("m_sharedRandomSeed")}


CBaseEntity["SetUsedBySurvivorsMask"] <- function(value){SetProp("m_iUsedBySurvivorsMask", value.tointeger())}
CBaseEntity["GetUsedBySurvivorsMask"] <- function(){return GetProp("m_iUsedBySurvivorsMask")}
	
CBaseEntity["SetWeaponID"] <- function(value){SetProp("m_weaponID", value.tointeger())}
CBaseEntity["GetWeaponID"] <- function(){return GetProp("m_weaponID")}

CBaseEntity["SetItemCount"] <- function(value){SetProp("m_itemCount", value.tointeger())}
CBaseEntity["GetItemCount"] <- function(){return GetProp("m_itemCount")}


CBaseEntity["SetWidth"] <- function(value){SetProp("m_flWidth", value.tofloat())}
CBaseEntity["GetWidth"] <- function(){return GetProp("m_flWidth")}

CBaseEntity["SetHeight"] <- function(value){SetProp("m_flHeight", value.tofloat())}
CBaseEntity["GetHeight"] <- function(){return GetProp("m_flHeight")}

CBaseEntity["SetAttachmentIndex"] <- function(value){SetProp("m_nAttachmentIndex", value.tointeger())}
CBaseEntity["GetAttachmentIndex"] <- function(){return GetProp("m_nAttachmentIndex")}

CBaseEntity["SetPanelName"] <- function(value){SetProp("m_nPanelName", value.tointeger())}
CBaseEntity["GetPanelName"] <- function(){return GetProp("m_nPanelName")}

CBaseEntity["SetScreenFlags"] <- function(value){SetProp("m_fScreenFlags", value.tointeger())}
CBaseEntity["GetScreenFlags"] <- function(){return GetProp("m_fScreenFlags")}

CBaseEntity["SetOverlayMaterial"] <- function(value){SetProp("m_nOverlayMaterial", value.tointeger())}
CBaseEntity["GetOverlayMaterial"] <- function(){return GetProp("m_nOverlayMaterial")}

CBaseEntity["SetPlayerOwner"] <- function(value){SetPropEntity("m_hPlayerOwner", value)}
CBaseEntity["GetPlayerOwner"] <- function(value){return GetPropEntity("m_hPlayerOwner")}


CBaseEntity["SetEnabled"] <- function(value){SetProp("m_bEnabled", value.tointeger())}
CBaseEntity["GetEnabled"] <- function(){return GetProp("m_bEnabled")}

CBaseEntity["SetDisplayText"] <- function(value){SetProp("m_szDisplayText", value.tostring())}
CBaseEntity["GetDisplayText"] <- function(){return GetProp("m_szDisplayText")}

CBaseEntity["SetSlideshowDirectory"] <- function(value){SetProp("m_szSlideshowDirectory", value.tostring())}
CBaseEntity["GetSlideshowDirectory"] <- function(){return GetProp("m_szSlideshowDirectory")}

CBaseEntity["SetMinSlideTime"] <- function(value){SetProp("m_fMinSlideTime", value.tofloat())}
CBaseEntity["GetMinSlideTime"] <- function(){return GetProp("m_fMinSlideTime")}

CBaseEntity["SetMaxSlideTime"] <- function(value){SetProp("m_fMaxSlideTime", value.tofloat())}
CBaseEntity["GetMaxSlideTime"] <- function(){return GetProp("m_fMaxSlideTime")}

CBaseEntity["SetCycleType"] <- function(value){SetProp("m_iCycleType", value.tointeger())}
CBaseEntity["GetCycleType"] <- function(){return GetProp("m_iCycleType")}

CBaseEntity["SetNoListRepeats"] <- function(value){SetProp("m_bNoListRepeats", value.tointeger())}
CBaseEntity["GetNoListRepeats"] <- function(){return GetProp("m_bNoListRepeats")}
	
CBaseEntity["SetScreenWidth"] <- function(value){SetProp("m_iScreenWidth", value.tointeger())}
CBaseEntity["GetScreenWidth"] <- function(){return GetProp("m_iScreenWidth")}

CBaseEntity["SetScreenHeight"] <- function(value){SetProp("m_iScreenHeight", value.tointeger())}
CBaseEntity["GetScreenHeight"] <- function(){return GetProp("m_iScreenHeight")}


CBaseEntity["SetActiveIssueIndex"] <- function(value){SetProp("m_activeIssueIndex", value.tointeger())}
CBaseEntity["GetActiveIssueIndex"] <- function(){return GetProp("m_activeIssueIndex")}

CBaseEntity["SetOnlyTeamToVote"] <- function(value){SetProp("m_onlyTeamToVote", value.tointeger())}
CBaseEntity["GetOnlyTeamToVote"] <- function(){return GetProp("m_onlyTeamToVote")}

CBaseEntity["SetVotesYes"] <- function(value){SetProp("m_votesYes", value.tointeger())}
CBaseEntity["GetVotesYes"] <- function(){return GetProp("m_votesYes")}

CBaseEntity["SetVotesNo"] <- function(value){SetProp("m_votesNo", value.tointeger())}
CBaseEntity["GetVotesNo"] <- function(){return GetProp("m_votesNo")}

CBaseEntity["SetPotentialVotes"] <- function(value){SetProp("m_potentialVotes", value.tointeger())}
CBaseEntity["GetPotentialVotes"] <- function(){return GetProp("m_potentialVotes")}


CBaseEntity["SetCheapWaterStartDistance"] <- function(value){SetProp("m_flCheapWaterStartDistance", value.tofloat())}
CBaseEntity["GetCheapWaterStartDistance"] <- function(){return GetProp("m_flCheapWaterStartDistance")}

CBaseEntity["SetCheapWaterEndDistance"] <- function(value){SetProp("m_flCheapWaterEndDistance", value.tofloat())}
CBaseEntity["GetCheapWaterEndDistance"] <- function(){return GetProp("m_flCheapWaterEndDistance")}


CBaseEntity["SetWeaponID"] <- function(value){SetProp("m_weaponID", value.tointeger())}
CBaseEntity["GetWeaponID"] <- function(){return GetProp("m_weaponID")}
	
CBaseEntity["SetItemCount"] <- function(value){SetProp("m_itemCount", value.tointeger())}
CBaseEntity["GetItemCount"] <- function(){return GetProp("m_itemCount")}


CBaseEntity["SetState"] <- function(value){SetProp("m_iState", value.tointeger())}
CBaseEntity["GetState"] <- function(){return GetProp("m_iState")}

CBaseEntity["SetHitting"] <- function(value){SetProp("m_bHitting", value.tointeger())}
CBaseEntity["GetHitting"] <- function(){return GetProp("m_bHitting")}


CBaseEntity["SetVulnerableToSpit"] <- function(value){SetProp("m_bVulnerableToSpit", value.tointeger())}
CBaseEntity["GetVulnerableToSpit"] <- function(){return GetProp("m_bVulnerableToSpit")}


CBaseEntity["SetLastAttackTime"] <- function(value){SetProp("m_flLastAttackTime", value.tofloat())}
CBaseEntity["GetLastAttackTime"] <- function(){return GetProp("m_flLastAttackTime")}

CBaseEntity["SetMeleeWeaponInfo"] <- function(value){SetPropEntity("m_hMeleeWeaponInfo", value)}
CBaseEntity["GetMeleeWeaponInfo"] <- function(value){return GetPropEntity("m_hMeleeWeaponInfo")}

CBaseEntity["SetNextPrimaryAttackIndex"] <- function(value){SetProp("m_iNextPrimaryAttackIndex", value.tointeger())}
CBaseEntity["GetNextPrimaryAttackIndex"] <- function(){return GetProp("m_iNextPrimaryAttackIndex")}

CBaseEntity["SetNextStrongAttackIndex"] <- function(value){SetProp("m_iNextStrongAttackIndex", value.tointeger())}
CBaseEntity["GetNextStrongAttackIndex"] <- function(){return GetProp("m_iNextStrongAttackIndex")}

CBaseEntity["SetNextSecondaryAttackIndex"] <- function(value){SetProp("m_iNextSecondaryAttackIndex", value.tointeger())}
CBaseEntity["GetNextSecondaryAttackIndex"] <- function(){return GetProp("m_iNextSecondaryAttackIndex")}

CBaseEntity["SetInMeleeSwing"] <- function(value){SetProp("m_bInMeleeSwing", value.tointeger())}
CBaseEntity["GetInMeleeSwing"] <- function(){return GetProp("m_bInMeleeSwing")}

CBaseEntity["SetMeleeSwingDuration"] <- function(value){SetProp("m_meleeSwingTimer.m_duration", value.tofloat())}
CBaseEntity["GetMeleeSwingDuration"] <- function(value){return GetProp("m_meleeSwingTimer.m_duration")}

CBaseEntity["SetMeleeSwingTimestamp"] <- function(value){SetProp("m_meleeSwingTimer.m_timestamp", value.tofloat()())}
CBaseEntity["GetMeleeSwingTimestamp"] <- function(value){return GetProp("m_meleeSwingTimer.m_timestamp")}
	
CBaseEntity["SetMapSetScriptName"] <- function(value){SetProp("m_strMapSetScriptName", value.tostring())}
CBaseEntity["GetMapSetScriptName"] <- function(){return GetProp("m_strMapSetScriptName")}


CBaseEntity["SetRage"] <- function(value){SetProp("m_rage", value.tofloat())}
CBaseEntity["GetRage"] <- function(){return GetProp("m_rage")}

CBaseEntity["SetWanderRage"] <- function(value){SetProp("m_wanderrage", value.tofloat())}
CBaseEntity["GetWanderRage"] <- function(){return GetProp("m_wanderrage")}


CBaseEntity["SetWaveHeight"] <- function(value){SetProp("m_flWaveHeight", value.tofloat())}
CBaseEntity["GetWaveHeight"] <- function(){return GetProp("m_flWaveHeight")}

CBaseEntity["SetWorldMins"] <- function(value){SetProp("m_WorldMins", value)}
CBaseEntity["GetWorldMins"] <- function(){return GetProp("m_WorldMins")}

CBaseEntity["SetWorldMaxs"] <- function(value){SetProp("m_WorldMaxs", value)}
CBaseEntity["GetWorldMaxs"] <- function(){return GetProp("m_WorldMaxs")}

CBaseEntity["SetStartDark"] <- function(value){SetProp("m_bStartDark", value.tointeger())}
CBaseEntity["GetStartDark"] <- function(){return GetProp("m_bStartDark")}

CBaseEntity["SetMaxOccludeeArea"] <- function(value){SetProp("m_flMaxOccludeeArea", value.tofloat())}
CBaseEntity["GetMaxOccludeeArea"] <- function(){return GetProp("m_flMaxOccludeeArea")}

CBaseEntity["SetMinOccluderArea"] <- function(value){SetProp("m_flMinOccluderArea", value.tofloat())}
CBaseEntity["GetMinOccluderArea"] <- function(){return GetProp("m_flMinOccluderArea")}

CBaseEntity["SetMaxPropScreenSpaceWidth"] <- function(value){SetProp("m_flMaxPropScreenSpaceWidth", value.tofloat())}
CBaseEntity["GetMaxPropScreenSpaceWidth"] <- function(){return GetProp("m_flMaxPropScreenSpaceWidth")}

CBaseEntity["SetMinPropScreenSpaceWidth"] <- function(value){SetProp("m_flMinPropScreenSpaceWidth", value.tofloat())}
CBaseEntity["GetMinPropScreenSpaceWidth"] <- function(){return GetProp("m_flMinPropScreenSpaceWidth")}

CBaseEntity["SetDetailSpriteMaterial"] <- function(value){SetProp("m_iszDetailSpriteMaterial", value.tostring())}
CBaseEntity["GetDetailSpriteMaterial"] <- function(){return GetProp("m_iszDetailSpriteMaterial")}

CBaseEntity["SetStartMusicType"] <- function(value){SetProp("m_iStartMusicType", value.tointeger())}
CBaseEntity["GetStartMusicType"] <- function(){return GetProp("m_iStartMusicType")}

CBaseEntity["SetMusicPostFix"] <- function(value){SetProp("m_iszMusicPostFix", value.tostring())}
CBaseEntity["GetMusicPostFix"] <- function(){return GetProp("m_iszMusicPostFix")}

CBaseEntity["SetColdWorld"] <- function(value){SetProp("m_bColdWorld", value.tointeger())}
CBaseEntity["GetColdWorld"] <- function(){return GetProp("m_bColdWorld")}
	
CBaseEntity["SetChapterTitle"] <- function(value){SetProp("m_iszChapterTitle", value.tostring())}
CBaseEntity["GetChapterTitle"] <- function(){return GetProp("m_iszChapterTitle")}

CBaseEntity["SetDisplayTitle"] <- function(value){SetProp("m_bDisplayTitle", value.tointeger())}
CBaseEntity["GetDisplayTitle"] <- function(){return GetProp("m_bDisplayTitle")}

CBaseEntity["SetTimeOfDay"] <- function(value){SetProp("m_iTimeOfDay", value.tointeger())}
CBaseEntity["GetTimeOfDay"] <- function(){return GetProp("m_iTimeOfDay")}



CBaseAnimating["GetMoveType"] <- function(){return GetPropInt("movetype")}
CBaseAnimating["SetMoveType"] <- function(type){SetPropInt("movetype", type)}

CBaseAnimating["PlaySound"] <- function(soundName){EmitSoundOn(soundName, this)}
CBaseAnimating["StopSound"] <- function(soundName){StopSoundOn(soundName, this)}

CBaseAnimating["GetFlags"] <- function(){return GetPropInt("m_fFlags")}
CBaseAnimating["SetFlags"] <- function(flag){SetPropInt("m_fFlags", flag)}
CBaseAnimating["AddFlag"] <- function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") | flag)}
CBaseAnimating["RemoveFlag"] <- function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") & ~flag)}
CBaseAnimating["HasFlag"] <- function(flag){return GetFlags() & flag}

CBaseAnimating["GetSpawnflags"] <- function(){return GetPropInt("m_spawnflags")}
CBaseAnimating["SetSpawnFlags"] <- function(flags){SetPropInt("m_spawnflags", flags)}

CBaseAnimating["GetGlowType"] <- function(){return GetPropInt("m_Glow.m_iGlowType")}
CBaseAnimating["SetGlowType"] <- function(type){SetPropInt("m_Glow.m_iGlowType", type)}

CBaseAnimating["GetGlowRange"] <- function(){return GetPropInt("m_Glow.m_nGlowRange")}
CBaseAnimating["SetGlowRange"] <- function(range){SetPropInt("m_Glow.m_nGlowRange", range)}

CBaseAnimating["GetGlowRangeMin"] <- function(){return GetPropInt("m_Glow.m_nGlowRangeMin")}
CBaseAnimating["SetGlowRangeMin"] <- function(range){SetPropInt("m_Glow.m_nGlowRangeMin", range)}

CBaseAnimating["GetGlowColor"] <- function(){return GetPropInt("m_Glow.m_glowColorOverride")}
CBaseAnimating["SetGlowColor"] <- function(r, g, b){
	local color = r
	color += 256 * g
	color += 65536 * b
	SetPropInt("m_Glow.m_glowColorOverride", color)
}
CBaseAnimating["SetGlowColorVector"] <- function(vector){
	local color = vector.x
	color += 256 * vector.y
	color += 65536 * vector.z
	SetPropInt("m_Glow.m_glowColorOverride", color)
}
CBaseAnimating["ResetGlowColor"] <- function(){SetPropInt("m_Glow.m_glowColorOverride", -1)}

CBaseAnimating["GetSequence"] <- function(){return GetPropInt("m_nSequence")}
CBaseAnimating["GetValidatedScriptScope"] <- function(){
	ValidateScriptScope()
	return GetScriptScope()
}

CBaseAnimating["SetTeam"] <- function(team){SetPropInt("m_iTeamNum", team.tointeger())}
CBaseAnimating["GetTeam"] <- function(){return GetPropInt("m_iTeamNum")}

CBaseAnimating["Input"] <- function(input, value = "", delay = 0, activator = null){DoEntFire("!self", input.tostring(), value.tostring(), delay.tofloat(), activator, this)}
CBaseAnimating["SetAlpha"] <- function(alpha){Input("Alpha", alpha)}
CBaseAnimating["Enable"] <- function(){Input("Enable")}
CBaseAnimating["Disable"] <- function(){Input("Disable")}

CBaseAnimating["GetMoveType"] <- function(){return GetPropInt("movetype")}
CBaseAnimating["SetMoveType"] <- function(type){SetPropInt("movetype", type)}

CBaseAnimating["GetModelIndex"] <- function(){return GetPropInt("m_nModelIndex")}
CBaseAnimating["GetModelName"] <- function(){return GetPropString("m_ModelName")}
CBaseAnimating["SetName"] <- function(name){SetPropString("m_iName", name)}

CBaseAnimating["GetFriction"] <- function(){return GetFriction(this)}

CBaseAnimating["HasProp"] <- function(propertyName){return NetProps.HasProp(this, propertyName)}
CBaseAnimating["GetPropType"] <- function(propertyName){return NetProps.GetPropType(this, propertyName)}
CBaseAnimating["GetPropArraySize"] <- function(propertyName){return NetProps.GetPropArraySize(this, propertyName)}

CBaseAnimating["GetPropInt"] <- function(propertyName){return NetProps.GetPropInt(this, propertyName)}
CBaseAnimating["GetPropEntity"] <- function(propertyName){return NetProps.GetPropEntity(this, propertyName)}
CBaseAnimating["GetPropString"] <- function(propertyName){return NetProps.GetPropString(this, propertyName)}
CBaseAnimating["GetPropFloat"] <- function(propertyName){return NetProps.GetPropFloat(this, propertyName)}
CBaseAnimating["GetPropVector"] <- function(propertyName){return NetProps.GetPropVector(this, propertyName)}
CBaseAnimating["SetPropInt"] <- function(propertyName, value){return NetProps.SetPropInt(this, propertyName, value)}
CBaseAnimating["SetPropEntity"] <- function(propertyName, value){return NetProps.SetPropEntity(this, propertyName, value)}
CBaseAnimating["SetPropString"] <- function(propertyName, value){return NetProps.SetPropString(this, propertyName, value)}
CBaseAnimating["SetPropFloat"] <- function(propertyName, value){return NetProps.SetPropFloat(this, propertyName, value)}
CBaseAnimating["SetPropVector"] <- function(propertyName, value){return NetProps.SetPropVector(this, propertyName, value)}

CBaseAnimating["GetPropIntArray"] <- function(propertyName, index){return NetProps.GetPropIntArray(this, propertyName, index)}
CBaseAnimating["GetPropEntityArray"] <- function(propertyName, index){return NetProps.GetPropEntityArray(this, propertyName, index)}
CBaseAnimating["GetPropStringArray"] <- function(propertyName, index){return NetProps.GetPropStringArray(this, propertyName, index)}
CBaseAnimating["GetPropFloatArray"] <- function(propertyName, index){return NetProps.GetPropFloatArray(this, propertyName, index)}
CBaseAnimating["GetPropVectorArray"] <- function(propertyName, index){return NetProps.GetPropVectorArray(this, propertyName, index)}
CBaseAnimating["SetPropIntArray"] <- function(propertyName, index, value){return NetProps.SetPropIntArray(this, propertyName, value, index)}
CBaseAnimating["SetPropEntityArray"] <- function(propertyName, index, value){return NetProps.SetPropEntityArray(this, propertyName, value, index)}
CBaseAnimating["SetPropStringArray"] <- function(propertyName, index, value){return NetProps.SetPropStringArray(this, propertyName, value, index)}
CBaseAnimating["SetPropFloatArray"] <- function(propertyName, index, value){return NetProps.SetPropFloatArray(this, propertyName, value, index)}
CBaseAnimating["SetPropVectorArray"] <- function(propertyName, index, value){return NetProps.SetPropVectorArray(this, propertyName, value, index)}

CBaseAnimating["SetProp"] <- function(propertyName, value, index = null){
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
CBaseAnimating["GetProp"] <- function(propertyName, index = null, asArray = false){
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

CTerrorPlayer["HasProp"] <- function(propertyName){return NetProps.HasProp(this, propertyName)}
CTerrorPlayer["GetPropType"] <- function(propertyName){return NetProps.GetPropType(this, propertyName)}
CTerrorPlayer["GetPropArraySize"] <- function(propertyName){return NetProps.GetPropArraySize(this, propertyName)}

CTerrorPlayer["GetPropInt"] <- function(propertyName){return NetProps.GetPropInt(this, propertyName)}
CTerrorPlayer["GetPropEntity"] <- function(propertyName){return NetProps.GetPropEntity(this, propertyName)}
CTerrorPlayer["GetPropString"] <- function(propertyName){return NetProps.GetPropString(this, propertyName)}
CTerrorPlayer["GetPropFloat"] <- function(propertyName){return NetProps.GetPropFloat(this, propertyName)}
CTerrorPlayer["GetPropVector"] <- function(propertyName){return NetProps.GetPropVector(this, propertyName)}
CTerrorPlayer["SetPropInt"] <- function(propertyName, value){return NetProps.SetPropInt(this, propertyName, value)}
CTerrorPlayer["SetPropEntity"] <- function(propertyName, value){return NetProps.SetPropEntity(this, propertyName, value)}
CTerrorPlayer["SetPropString"] <- function(propertyName, value){return NetProps.SetPropString(this, propertyName, value)}
CTerrorPlayer["SetPropFloat"] <- function(propertyName, value){return NetProps.SetPropFloat(this, propertyName, value)}
CTerrorPlayer["SetPropVector"] <- function(propertyName, value){return NetProps.SetPropVector(this, propertyName, value)}

CTerrorPlayer["GetPropIntArray"] <- function(propertyName, index){return NetProps.GetPropIntArray(this, propertyName, index)}
CTerrorPlayer["GetPropEntityArray"] <- function(propertyName, index){return NetProps.GetPropEntityArray(this, propertyName, index)}
CTerrorPlayer["GetPropStringArray"] <- function(propertyName, index){return NetProps.GetPropStringArray(this, propertyName, index)}
CTerrorPlayer["GetPropFloatArray"] <- function(propertyName, index){return NetProps.GetPropFloatArray(this, propertyName, index)}
CTerrorPlayer["GetPropVectorArray"] <- function(propertyName, index){return NetProps.GetPropVectorArray(this, propertyName, index)}
CTerrorPlayer["SetPropIntArray"] <- function(propertyName, index, value){return NetProps.SetPropIntArray(this, propertyName, value, index)}
CTerrorPlayer["SetPropEntityArray"] <- function(propertyName, index, value){return NetProps.SetPropEntityArray(this, propertyName, value, index)}
CTerrorPlayer["SetPropStringArray"] <- function(propertyName, index, value){return NetProps.SetPropStringArray(this, propertyName, value, index)}
CTerrorPlayer["SetPropFloatArray"] <- function(propertyName, index, value){return NetProps.SetPropFloatArray(this, propertyName, value, index)}
CTerrorPlayer["SetPropVectorArray"] <- function(propertyName, index, value){return NetProps.SetPropVectorArray(this, propertyName, value, index)}

CTerrorPlayer["SetProp"] <- function(propertyName, value, index = null){
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
CTerrorPlayer["GetProp"] <- function(propertyName, index = null, asArray = false){
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

CTerrorPlayer["Input"] <- function(input, value = "", delay = 0, activator = null){DoEntFire("!self", input.tostring(), value.tostring(), delay.tofloat(), activator, this)}
CTerrorPlayer["SetAlpha"] <- function(alpha){Input("Alpha", alpha)}
CTerrorPlayer["Enable"] <- function(){Input("Enable")}
CTerrorPlayer["Disable"] <- function(){Input("Disable")}

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

CTerrorPlayer["GetValidatedScriptScope"] <- function(){
	ValidateScriptScope()
	return GetScriptScope()
}

CTerrorPlayer["CommandBot"] <- function(commandTable){
	commandTable["bot"] <- this
	CommandABot(commandTable)
}

CTerrorPlayer["GetMoveType"] <- function(){return GetPropInt("movetype")}
CTerrorPlayer["SetMoveType"] <- function(type){SetPropInt("movetype", type)}

CTerrorPlayer["GetFlags"] <- function(){return GetPropInt("m_fFlags")}
CTerrorPlayer["SetFlags"] <- function(flag){SetPropInt("m_fFlags", flag)}
CTerrorPlayer["AddFlag"] <- function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") | flag)}
CTerrorPlayer["RemoveFlag"] <- function(flag){SetPropInt("m_fFlags", GetPropInt("m_fFlags") & ~flag)}
CTerrorPlayer["HasFlag"] <- function(flag){return GetFlags() & flag}

CTerrorPlayer["GetGlowType"] <- function(){return GetPropInt("m_Glow.m_iGlowType")}
CTerrorPlayer["SetGlowType"] <- function(type){SetPropInt("m_Glow.m_iGlowType", type)}

CTerrorPlayer["GetGlowRange"] <- function(){return GetPropInt("m_Glow.m_nGlowRange")}
CTerrorPlayer["SetGlowRange"] <- function(range){SetPropInt("m_Glow.m_nGlowRange", range)}

CTerrorPlayer["GetGlowRangeMin"] <- function(){return GetPropInt("m_Glow.m_nGlowRangeMin")}
CTerrorPlayer["SetGlowRangeMin"] <- function(range){SetPropInt("m_Glow.m_nGlowRangeMin", range)}

CTerrorPlayer["GetGlowColor"] <- function(){return GetPropInt("m_Glow.m_glowColorOverride")}
CTerrorPlayer["SetGlowColor"] <- function(r, g, b){
	local color = r
	color += 256 * g
	color += 65536 * b
	SetPropInt("m_Glow.m_glowColorOverride", color)
}
CTerrorPlayer["SetGlowColorVector"] <- function(vector){
	local color = vector.x
	color += 256 * vector.y
	color += 65536 * vector.z
	SetPropInt("m_Glow.m_glowColorOverride", color)
}
CTerrorPlayer["ResetGlowColor"] <- function(){SetPropInt("m_Glow.m_glowColorOverride", -1)}

CTerrorPlayer["GetModelIndex"] <- function(){return GetPropInt("m_nModelIndex")}
CTerrorPlayer["GetModelName"] <- function(){return GetPropString("m_ModelName")}

CTerrorPlayer["SetName"] <- function(name){SetPropString("m_iName", name)}

CTerrorPlayer["GetFriction"] <- function(){return GetFriction(this)}

CTerrorPlayer["SetTeam"] <- function(team){SetPropInt("m_iTeamNum", team.tointeger())}
CTerrorPlayer["GetTeam"] <- function(){return GetPropInt("m_iTeamNum")}

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

CTerrorPlayer["SetReviveCount"] <- function(value){SetProp("m_currentReviveCount", value.tointeger())}
CTerrorPlayer["GetReviveCount"] <- function(){return GetPropInt("m_currentReviveCount")}

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

CTerrorPlayer["PlaySound"] <- function(soundName){EmitSoundOn(soundName, this)}
CTerrorPlayer["StopSound"] <- function(soundName){StopSoundOn(soundName, this)}

CTerrorPlayer["PlaySoundOnClient"] <- function(soundName){EmitSoundOnClient(soundName, this)}

CTerrorPlayer["GetAmmo"] <- function(weapon){return GetPropIntArray("m_iAmmo", weapon.GetPropInt("m_iPrimaryAmmoType"))}
CTerrorPlayer["SetAmmo"] <- function(weapon, ammo){SetPropIntArray("m_iAmmo", weapon.GetPropInt("m_iPrimaryAmmoType"), ammo)}