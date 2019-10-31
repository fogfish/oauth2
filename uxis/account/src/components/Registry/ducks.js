import { APPLICATION } from '../NewApp/ducks'
import { fail } from '../Notification/ducks'

const REVOKE   = "@@registry/revoke"
const REGISTRY = "@@registry/registry"

//
//
export const lookup = () =>
  async (dispatch, getState) => {
    try {
      const apps = await fetch('/oauth2/client', {
        method: 'GET',
        headers: {
          "Authorization": getState().oauth2.token
        }
      }).then(jsonify)
      dispatch({type: REGISTRY, registry: apps})
    } catch (e) {
      dispatch(fail(e))
      dispatch({type: REGISTRY, registry: []})
    }
  }

const jsonify = (http) => {
   if (http.ok) {
      return http.json()
   } else {
      return http.json().then(error => Promise.reject(error))
   }
}

//
//
export const revoke = (access) =>
  async (dispatch, getState) => {
    try {
      await fetch(`/oauth2/client/${access}`, {
        method: 'DELETE',
        headers: {
          "Authorization": getState().oauth2.token
        }
      }).then(jsonify)

      dispatch({type: REVOKE, access})
    } catch (e) {
      dispatch(fail(e))
    }
  }


//
//
const empty = {
  apps: undefined
}

export default (state = empty, action) => {
  switch (action.type) {
    case APPLICATION:
      const name = action.name
      const access = action.access
      const redirect_uri = action.redirect_uri
      const security = action.security
      return { ...state, apps: state.apps.concat([{name, access, redirect_uri, security}])}
    case REGISTRY:
      return { ...state, apps: action.registry}
    case REVOKE:
      return { ...state, apps: state.apps.filter(x => x.access !== action.access)}
    default:
      return { ...state }
  }
}   