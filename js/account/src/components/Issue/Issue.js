import React from 'react'
import { NonIdealState, Button, Intent } from '@blueprintjs/core'
import { IconNames } from '@blueprintjs/icons'
import { authorize } from '../OAuth2'

const Unauthorized = () => (
  <NonIdealState
    icon={IconNames.USER}
    title="Ops, Sorry!"
    description="Your session is expired!"
    action={<Button large intent={Intent.PRIMARY} onClick={authorize}>Please Sign-In!</Button>}
  />
)

const NotFound = () => (
  <NonIdealState
    icon={IconNames.DOCUMENT}
    title="File Not Found!"
    description={<p className="bp3-text-large">Unable to find requested file.</p>}
  />
)

const Unknown = ({ type, title }) => (
  <NonIdealState
    icon={IconNames.ERROR}
    title="Ops, Sorry!"
    description={`Our server fails with '${title}' error. See details at ${type}.`}
  />
)

export default ({ status }) => {
  switch (status.reason.type) {
    case 'https://httpstatuses.com/401':
      return (<Unauthorized />)

    case 'https://httpstatuses.com/404':
      return (<NotFound />)

    default:
      return (<Unknown type={status.reason.type} title={status.reason.title} />)
  }
}
