
import {signout} from './access-token'
import {showSecretKeys, showLoading, hideLoading, showFailure} from './core'
import {appendOAuthApp, removeOAuthApp} from './account'

//
//
const empty = {
   name: null,          // oauth application name
   security: null,      // oauth application security profile
   redirect_uri: null,  // oauth application redirect uri

   keys: {}
}

const APP_INIT = "@@oauth-app/init"
const APP_NAME = "@@oauth-app/name";
const APP_TYPE = "@@oauth-app/type";
const APP_REDIRECT_URI = "@@oauth-app/redirect_uri";
const APP_KEYS = "@@oauth-app/keys";

//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case APP_INIT:
      return {...state, name: "", security: "public", redirect_uri: ""};   
   case APP_NAME:
      return {...state, name: action.value};
   case APP_TYPE:
      return {...state, security: action.value};
   case APP_REDIRECT_URI:
      return {...state, redirect_uri: action.value};

   case APP_KEYS:
      return {...state, keys: action.keys}

   default:
      return state
   }
}


//
//
export const onAppName = (e) => ({type: APP_NAME, value: e.target.value})
export const onAppSecurity = (e) => ({type: APP_TYPE, value: e.target.value})
export const onAppRedirectUri = (e) => ({type: APP_REDIRECT_URI, value: e.target.value})

export const onAppKeys = (keys) => ({type: APP_KEYS, keys})


//
//
export const createOAuthApp = () => ({type: APP_INIT})

//
//
export const registerOAuthApp = () =>
   (dispatch, getState) => {
      dispatch(showLoading())
      fetch('/oauth2/client', {
         method: 'POST',
         headers: {
            "Authorization": getState().access_token.bearer
         },
         body: JSON.stringify(getState().app)
      }).then(
         ioHttp
      ).then(
         (keys) => {
            const name = getState().app.name;
            const security = getState().app.security;
            const redirect_uri = getState().app.redirect_uri;
            const access = keys.access
            dispatch(hideLoading())
            dispatch(appendOAuthApp({access, name, security, redirect_uri}))
            dispatch(onAppKeys(keys))
            dispatch(showSecretKeys())
         }
      ).catch(
         (error) => {
            dispatch(hideLoading())
            ioError(dispatch, error)
         }
      )
   }

//
//
export const revokeOAuthApp = (app) =>
   (dispatch, getState) => {
      dispatch(showLoading())
      fetch('/oauth2/client/' + app.access, {
         method: 'DELETE',
         headers: {
            "Authorization": getState().access_token.bearer
         }
      }).then(
         ioHttp
      ).then(
         (_json) => {
            dispatch(hideLoading())
            dispatch(removeOAuthApp(app))
         }
      ).catch(
         (error) => {
            dispatch(hideLoading())
            ioError(dispatch, error)
         }
      )
   }

//
//
export const lookupOAuthApp = () =>
   (dispatch, getState) => {
      if (!getState().access_token.bearer)
         return 
      
      dispatch(showLoading())
      fetch('/oauth2/client', {
         method: 'GET',
         headers: {
            "Authorization": getState().access_token.bearer
         }
      }).then(
         ioHttp
      ).then(
         (list) => {
            dispatch(hideLoading())
            list.map((x) => dispatch(appendOAuthApp(x)))
         }
      ).catch(
         (error) => {
            dispatch(hideLoading())
            ioError(dispatch, error)
         }
      )
   }

//
//
const ioHttp = (http) => {
   if (http.ok) {
      return http.json()
   } else {
      return http.json().then(error => Promise.reject(error))
   }
}


//
//
const ioError = (dispatch, json) => {
   switch (json.type) {

   case "https://httpstatuses.com/401": // Unauthorized
      dispatch(signout())
      return;

   case "https://httpstatuses.com/500":
      switch (json.details) {
      case "invalid_uri":
         dispatch(showFailure("Invalid redirect URI. It must define schema, authority and path (http://www.example.com/redirect.html). "))
         return;
      default:
         dispatch(showFailure("Unable to execute operation. Try again later."))
         return;
      }

   default: 
      dispatch(showFailure("Unable to execute operation. Try again later."))
      return;
   }
}





