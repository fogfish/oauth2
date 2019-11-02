import React from 'react'

export const PENDING = 'pending'
export const FAILURE = 'failure'
export const SUCCESS = 'success'

export const WhileIO = (Loading, Recover, Component) => ({status, ...props}) => {
  if (status === PENDING) {
    return (!Loading ? null : <Loading status={status} {...props} />)
  }

  if (status === FAILURE) {
    return (!Recover ? null : <Recover status={status} {...props} />)
  }

  return <Component status={status} {...props} />
}
