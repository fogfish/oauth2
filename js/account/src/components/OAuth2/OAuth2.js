import React, { useState, useEffect, useMemo } from 'react'

//
// Global OAuth2 configuration
const OAUTH2_AUTHORIZE = process.env.REACT_APP_OAUTH2_AUTHORIZE
const OAUTH2_TOKEN = process.env.REACT_APP_OAUTH2_TOKEN
const OAUTH2_TRYOUT = process.env.REACT_APP_OAUTH2_TRYOUT
const OAUTH2_CLIENT_ID = process.env.REACT_APP_OAUTH2_CLIENT_ID
const OAUTH2_FLOW_TYPE = process.env.REACT_APP_OAUTH2_FLOW_TYPE
const OAUTH2_SCOPE = process.env.REACT_APP_OAUTH2_SCOPE

//
const PENDING = 'pending'
const FAILURE = 'failure'
const SUCCESS = 'success'

//
// 
export const authorize = () => {
  const request = {
    client_id: OAUTH2_CLIENT_ID,
    response_type: OAUTH2_FLOW_TYPE,
    scope: OAUTH2_SCOPE,
    state: 'none',
  }
  window.localStorage.removeItem('access_token')
  window.localStorage.removeItem('access_token_bearer')
  window.location = `${OAUTH2_AUTHORIZE}/?${encode(request)}`
}

//
//
export const tryout = () => {
  window.localStorage.removeItem('access_token')
  window.localStorage.removeItem('access_token_bearer')
  window.location = `${OAUTH2_TRYOUT}`
}

//
//
export const signout = () => {
  window.localStorage.removeItem('access_token')
  window.localStorage.removeItem('access_token_bearer')
  window.location = '/'
}

//
export const WhoIs = (Component) => {
  const whois = useMemo(() => (accessTokenStorage()))

  return (<Component {...whois} />)
}

//
//
export const useOAuth2 = () => {
  const [status, updateStatus] = useState({ status: PENDING, error: undefined })

  useEffect(() => {
    accessToken(updateStatus)
  }, [])

  return status
}

//
//
const accessToken = async (updateStatus) => {
  updateStatus({ status: PENDING, error: undefined })
  const oauth2 = decode(window.location.search.substring(1))

  window.history.replaceState({}, document.title, window.location.pathname + window.location.hash)

  if (oauth2.error) {
    updateStatus({ status: FAILURE, error: oauth2.error })
  } else if (oauth2.access_token) {
    const token = accessTokenImplicit(oauth2)
    !isValidToken(token)
      ? updateStatus({ status: FAILURE, error: tokenIssue(token) })
      : refresh(token, updateStatus)
  } else if (oauth2.code) {
    try {
      const token = await accessTokenExchange(oauth2)
      !isValidToken(token)
        ? updateStatus({ status: FAILURE, error: tokenIssue(token) })
        : refresh(token, updateStatus)
    } catch (e) {
      updateStatus({ status: FAILURE, error: e })
    }
  } else {
    const token = accessTokenStorage()
    !isValidToken(token)
      ? updateStatus({ status: FAILURE, error: tokenIssue(token) })
      : refresh(token, updateStatus) 
  }
}

const refresh = ({ expires }, updateStatus) => {
  const now = +new Date()
  const timeout = Math.min(expires - now, 15 * 86400 * 1000)
  setTimeout(() => updateStatus({ status: FAILURE, error: 'expired'}), timeout)
  updateStatus({ status: SUCCESS, error: undefined })
}

//
// encode / decode utilities
const keyval = (key, val) => (key === '' ? val : decodeURIComponent(val))
const decode = text => (text ? JSON.parse(`{"${text.replace(/&/g, '","').replace(/=/g, '":"')}"}`, keyval) : {})
const encode = json =>
  Object.keys(json).map(key => ([encodeURIComponent(key), encodeURIComponent(json[key])].join('='))).join('&')

//
//
const isValidToken = ({ token, expires, acc }) => {
  const now = +new Date()
  return (token && acc && expires > now)
}

const tokenIssue = ({ token, expires, acc }) => {
  const now = +new Date()
  if (token && !acc && expires > now)
    return 'standby'

  if (token && acc && expires <= now)
    return 'expired'

  return 'unauthorized'
}

//
//
const accessTokenStorage = () => {
  try {
    const token = window.localStorage.getItem('access_token')
    return token ? JSON.parse(token) : {}
  } catch (_) {
    return {}
  }
}

//
//
const accessTokenImplicit = (oauth2) => {
  const { token_type, access_token, expires_in, ...scopes } = oauth2
  const now = +new Date()
  const token = `Bearer ${access_token}`
  const expires = now + expires_in * 1000 - 60000
  const rights = { token, expires, ...scopes }
  window.localStorage.setItem('access_token', JSON.stringify(rights))
  window.localStorage.setItem('access_token_bearer', token)
  return rights
}

//
//
const accessTokenExchange = async oauth2 => (
  fetch(OAUTH2_TOKEN, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: encode({ grant_type: 'authorization_code', client_id: OAUTH2_CLIENT_ID, code: oauth2.code }),
  })
    .then(jsonify('application/json'))
    .then(accessTokenImplicit)
)

const jsonify = contentType => async http => {
  if (http.status >= 200 && http.status < 300) {
    return contentType === 'application/json' ? http.json() : http.text()
  } else {
    const error = await http.json()
    throw new Issue(http, error)
  }
}



//
// Network I/O
export const secureIO = (url, { headers = {}, ...spec }) => 
  fetch(
    url, 
    { 
      ...spec, 
      headers:
      {
        ...headers,
        Authorization: window.localStorage.getItem('access_token_bearer'),
      }
    }
  )
    .catch(recoverIncorrectCORS)
    .then(jsonify(headers['Accept']))


const recoverIncorrectCORS = (error) => {
  // early catch TypeError to handle 5xx errors without proper CORS header
  // convert them to network Issue to distinguish at UI recovery
  if (error instanceof TypeError) {
    throw new Issue(
      {status: 500},
      {title: 'Unable to load remote resource due to access control check'},
    )
  }
  throw error
}


export const secureLookup = url => secureIO(url, {method: 'GET', headers: { 'Accept': 'application/json' } })

export const secureCreate = (url, json) => secureIO(url, { 
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
  },
  body: JSON.stringify(json)
})

export const secureUpdate = (url, json) => secureIO(url, { 
  method: 'PUT', 
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
  },
  body: JSON.stringify(json)
})

export const secureRemove = url => secureIO(url, { method: 'DELETE', headers: { 'Accept': 'application/json' } })

export class Issue extends Error {
  constructor(http, json) {
    super(http.status)
    this.type = json.type
    this.instance = json.instance
    this.title = json.title
    this.details = json.details
  }
}