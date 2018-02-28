import React from 'react'
import { Page, Container, Row, Column, Card } from 'react-dress-code'
import Header from './Header'
import Menu from './Menu'
import { Registry } from '../Registry'


const Account = (props) => (
  <Page>
    <Container limited>
      <Header />
      <Row>
        <Column large={2} medium={2}><Menu /></Column>
        <Column large={10} medium={10}><Registry { ...props } /></Column>
      </Row>
    </Container>
  </Page>
)

export default Account
