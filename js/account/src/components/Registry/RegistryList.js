import React from 'react'
import { Code, AnchorButton, Intent, Icon } from '@blueprintjs/core'
import { useSecureRemove, PENDING, SUCCESS, FAILURE, unknown } from '../OAuth2'

const OAUTH2_CLIENT = process.env.REACT_APP_OAUTH2_CLIENT

const Item = ({ app, access, security, redirect_uri, revoke }) => {
  const { status, retry, sequence } = useSecureRemove(undefined)
  
  if (status instanceof SUCCESS) {
    revoke(access)
  }

  return (status.status instanceof SUCCESS ? null :
    <tr>
      <td data-col="Application">{app}</td>
      <td data-col="Client ID"><Code>{access}</Code></td>
      <td data-col="Security">{security}</td>
      <td data-col="Redirect">{redirect_uri}</td>
      <td>
        <AnchorButton
          intent={Intent.DANGER}
          minimal
          loading={status instanceof PENDING}
          onClick={() =>
            status instanceof FAILURE
              ? retry()
              : sequence(`${OAUTH2_CLIENT}/${access}`)
          }
        >
          revoke
        </AnchorButton>
      </td>
      <td>
        {status instanceof FAILURE ? 
          <Icon intent={Intent.DANGER} icon='warning-sign' /> : 
          <Icon icon='' />
        }
      </td>
    </tr>
  )
}


const RegistryList = ({ content, revoke }) => (
  <table className="bp3-html-table bp3-interactive" style={{width: '100%'}}>
    <thead>
      <tr>
        <th>Application</th>
        <th>Client ID</th>
        <th>Security</th>
        <th>Redirect Uri</th>
        <th></th>
        <th></th>
      </tr>
    </thead>
    <tbody>
      {content.map((x) => <Item key={x.access} {...x} revoke={revoke}/>)}
    </tbody>
  </table>
)

export default RegistryList
