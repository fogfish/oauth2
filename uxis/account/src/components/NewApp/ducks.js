import { fail } from '../Notification/ducks'

//
//
export const IDENTITY = "@@app/identity"
export const ENDPOINT = "@@app/endpoint"
export const SECURITY = "@@app/security"
export const KEYS     = "@@app/keys"
export const APPLICATION = "@@app/application"

//
//
export const identity = (e) =>
  (dispatch, getState) => 
    dispatch({type: IDENTITY, value: e.target.value})

//
//
export const endpoint = (e) =>
  (dispatch, getState) => 
    dispatch({type: ENDPOINT, value: e.target.value})

//
//
export const security = (e) =>
  (dispatch, getState) => 
    dispatch({type: SECURITY, value: e.target.value})

//
//
export const commit = () =>
  async (dispatch, getState) => {
    try {
      const keys = await fetch('/oauth2/client', {
        method: 'POST',
        headers: {
          "Authorization": getState().oauth2.token
        },
        body: JSON.stringify(spec(getState().app))
      }).then(jsonify)
      dispatch({type: KEYS, keys})
    } catch (e) {
      dispatch(fail(e))
    }
  }

const jsonify = (http) => {
   if (http.ok) {
      return http.json()
   } else {
      return http.json().then(error => Promise.reject(error))
   }
}

const spec = ({identity, endpoint, security}) => (
  {name: identity, redirect_uri: endpoint, security: security}
)

//
//
export const register = (history) =>
  (dispatch, getState) => {
    const app = spec(getState().app)
    dispatch({ ...app, type: APPLICATION, access: getState().app.keys.access})    
    history.goBack()
  }


//
//
const empty = {
  identity: undefined,
  endpoint: undefined,
  security: "public",
  keys: undefined
}

export default (state = empty, action) => {
  switch (action.type) {
    case IDENTITY:
      return { ...state, identity: action.value }
    case ENDPOINT:
      return { ...state, endpoint: action.value }
    case SECURITY:
      return { ...state, security: action.value }
    case KEYS:
      return { ...state, keys: action.keys }
    case APPLICATION:
      return { ...empty }
    default:
      return { ...state }
  }
}   