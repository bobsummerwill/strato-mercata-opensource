import dayjs from 'dayjs';

describe('Renders Orders Page', () => {
  it('it should create an order', () => {
    cy.intercept({
      method: 'GET',
      url: '/api/v1/product?isDeleted=false&category=Art&subCategory=Art',
    }).as('productNameCall');

    cy.visit('/');
    cy.get('#Login').click();
    cy.login();

    const productName = `Corn-Seeds-${dayjs().unix()}`;
    cy.createProduct(productName);
    cy.createInventory(productName);

    cy.get('#user-dropdown').click();
    cy.get('#logout').click();
    cy.get('#Orders').should('not.exist');
    cy.get('#Login').click();
    cy.loginAsSeller();

    cy.get('#Marketplace').should('exist');
    cy.get('#Marketplace').click();
    cy.url().should('include', '/marketplace');

    cy.get('#Art').should('exist');
    cy.get('#Art').click();

    cy.get(`#${productName}-buy-now`).should('exist');
    cy.get(`#${productName}-buy-now`).click();
    cy.url().should('include', '/marketplace/checkout');
    cy.get('#submit-order-button').should('exist');
    cy.get('#submit-order-button').click();
    cy.url().should('include', '/marketplace/confirmOrder');

    cy.request({
      method: 'GET',
      url: '/api/v1/order/userAddresses/user',
    }).then(({ status, body }) => {
      expect(status).to.eq(200);
      if (body.data.length == 0) {
        cy.get('input[placeholder="Enter Name"]').type('Shubham Dubey');
        cy.get('input[placeholder="Enter Zipcode"]').type('32545');
        cy.get('input[placeholder="Enter State"]').type('Dallas');
        cy.get('input[placeholder="Enter City"]').type('New york');
        cy.get('textarea[placeholder="Enter Address Line 1"]').type(
          'Street A, near block'
        );
        cy.get('#add-address-button').should('exist');
        cy.get('#add-address-button').click();
      }
    });
    cy.get('#pay-later-button').should('exist');
    cy.get('#pay-later-button').click();
    cy.get('#modal-title').contains('Confirm Order');
    cy.get('#yes-button').should('exist');
    cy.get('#yes-button').click();

    cy.contains('Order created successfully').should('be.visible');
    cy.url().should('include', '/orders');
  });

  it('it should able to cancel order as a buyer', () => {
    cy.intercept({
      method: 'POST',
      url: '/api/v1/order',
    }).as('ordersCall');

    cy.intercept({
      method: 'GET',
      url: '/api/v1/product?isDeleted=false&category=Art&subCategory=Art',
    }).as('productNameCall');

    cy.visit('/');
    cy.get('#Login').click();
    cy.login();

    const productName = `Corn-Seeds-${dayjs().unix()}`;
    cy.createProduct(productName);
    cy.createInventory(productName);

    cy.get('#user-dropdown').click();
    cy.get('#logout').click();
    cy.get('#Orders').should('not.exist');
    cy.get('#Login').click();
    cy.loginAsSeller();

    cy.get('#Marketplace').should('exist');
    cy.get('#Marketplace').click();
    cy.url().should('include', '/marketplace');

    cy.get('#Art').should('exist');
    cy.get('#Art').click();

    cy.get(`#${productName}-buy-now`).should('exist');
    cy.get(`#${productName}-buy-now`).click();
    cy.url().should('include', '/marketplace/checkout');
    cy.get('#submit-order-button').should('exist');
    cy.get('#submit-order-button').click();
    cy.url().should('include', '/marketplace/confirmOrder');

    cy.request({
      method: 'GET',
      url: '/api/v1/order/userAddresses/user',
    }).then(({ status, body }) => {
      console.log(body);
      expect(status).to.eq(200);
      if (body.data.length == 0) {
        cy.get('input[placeholder="Enter Name"]').type('Shubham Dubey');
        cy.get('input[placeholder="Enter Zipcode"]').type('32545');
        cy.get('input[placeholder="Enter State"]').type('Dallas');
        cy.get('input[placeholder="Enter City"]').type('New york');
        cy.get('textarea[placeholder="Enter Address Line 1"]').type(
          'Street A, near block'
        );
        cy.get('#add-address-button').should('exist');
        cy.get('#add-address-button').click();
      }
    });
    cy.get('#pay-later-button').should('exist');
    cy.get('#pay-later-button').click();
    cy.get('#modal-title').contains('Confirm Order');
    cy.get('#yes-button').should('exist');

    cy.get('#yes-button')
      .click()
      .then(() => {
        let orderAddress;
        cy.wait('@ordersCall', { timeout: 190000 })
          .its('response.body')
          .then((body) => {
            console.log(body);
            orderAddress = body.data[0][1];
            if (orderAddress) {
              cy.contains('Order created successfully').should('be.visible');
              cy.get('#Orders').click();
              cy.url().should('include', '/orders');
              cy.get('#bought-tab').should('exist');
              cy.get('#bought-tab').click();

              cy.visit(`/marketplace/bought-orders/${orderAddress}`);
              // cy.url().should(`include`, `/marketplace/bought-orders/${order.address}`);

              cy.get('textarea[placeholder="Enter Comments"]').type(
                'I want to cancel this order'
              );
              cy.get('#cancel-order-button').should('exist');
              cy.get('#cancel-order-button').click();

              cy.contains('Order has been updated').should('be.visible');
            }
          });
      });
  });

  it('it should able to cancel order as a seller', () => {
    cy.intercept({
      method: 'POST',
      url: '/api/v1/order',
    }).as('ordersCall');

    cy.intercept({
      method: 'GET',
      url: '/api/v1/product?isDeleted=false&category=Art&subCategory=Art',
    }).as('productNameCall');

    cy.visit('/');
    cy.get('#Login').click();
    cy.login();

    const productName = `Corn-Seeds-${dayjs().unix()}`;
    cy.createProduct(productName);
    cy.createInventory(productName);

    cy.get('#user-dropdown').click();
    cy.get('#logout').click();
    cy.get('#Orders').should('not.exist');
    cy.get('#Login').click();
    cy.loginAsSeller();

    cy.get('#Art').click();
    cy.get('#Marketplace').should('exist');
    cy.get('#Marketplace').click();
    cy.url().should('include', '/marketplace');

    cy.get('#Art').should('exist');
    cy.get('#Art').click();

    cy.get(`#${productName}-buy-now`).should('exist');
    cy.get(`#${productName}-buy-now`).click();
    cy.url().should('include', '/marketplace/checkout');
    cy.get('#submit-order-button').should('exist');
    cy.get('#submit-order-button').click();
    cy.url().should('include', '/marketplace/confirmOrder');

    cy.request({
      method: 'GET',
      url: '/api/v1/order/userAddresses/user',
    }).then(({ status, body }) => {
      console.log(body);
      expect(status).to.eq(200);
      if (body.data.length == 0) {
        cy.get('input[placeholder="Enter Name"]').type('Shubham Dubey');
        cy.get('input[placeholder="Enter Zipcode"]').type('32545');
        cy.get('input[placeholder="Enter State"]').type('Dallas');
        cy.get('input[placeholder="Enter City"]').type('New york');
        cy.get('textarea[placeholder="Enter Address Line 1"]').type(
          'Street A, near block'
        );
        cy.get('#add-address-button').should('exist');
        cy.get('#add-address-button').click();
      }
    });
    cy.get('#pay-later-button').should('exist');
    cy.get('#pay-later-button').click();
    cy.get('#modal-title').contains('Confirm Order');
    cy.get('#yes-button').should('exist');
    // cy.get("#yes-button").click();

    cy.get('#yes-button')
      .click()
      .then(() => {
        let orderAddress;
        cy.wait('@ordersCall', { timeout: 60000 })
          .its('response.body')
          .then((body) => {
            console.log(body);
            orderAddress = body.data[0][1];
            if (orderAddress) {
              cy.contains('Order created successfully').should('be.visible');
              cy.get('#user-dropdown').click();
              cy.get('#logout').click();
              cy.get('#Orders').should('not.exist');

              cy.visit('/');
              cy.get('#Login').click();
              cy.login();

              cy.get('#Orders').should('exist');
              cy.visit(`/marketplace/sold-orders/${orderAddress}`);

              cy.get('textarea[placeholder="Enter Comments"]').type(
                'I want to close this order'
              );
              // cy.get('.ant-select-selector').click().select("Canceled")
              cy.get('.ant-select-selector').click();
              cy.contains(
                '.ant-select-item-option-content',
                'Canceled'
              ).click();
              cy.get('#yes-button').should('exist');
              cy.get('#yes-button').click();

              cy.contains('Order has been updated').should('be.visible');
            }
          });
      });
  });

  it('it should allow seller to change the order status to closed', () => {
    cy.intercept({
      method: 'POST',
      url: '/api/v1/order',
    }).as('ordersCall');

    cy.intercept({
      method: 'GET',
      url: '/api/v1/product?isDeleted=false&category=Art&subCategory=Art',
    }).as('productNameCall');

    cy.visit('/');
    cy.get('#Login').click();
    cy.loginAsSeller();

    const productName = `Corn-Seeds-${dayjs().unix()}`;
    cy.createProduct(productName);
    cy.createInventory(productName);

    cy.get('#user-dropdown').click();
    cy.get('#logout').click();
    cy.get('#Orders').should('not.exist');
    cy.get('#Login').click();
    cy.login();

    cy.get('#Marketplace').should('exist');
    cy.get('#Marketplace').click();
    cy.url().should('include', '/marketplace');

    cy.get('#Art').should('exist');
    cy.get('#Art').click();

    cy.get(`#${productName}-buy-now`).should('exist');
    cy.get(`#${productName}-buy-now`).click();
    cy.url().should('include', '/marketplace/checkout');
    cy.get('#submit-order-button').should('exist');
    cy.get('#submit-order-button').click();
    cy.url().should('include', '/marketplace/confirmOrder');

    cy.request({
      method: 'GET',
      url: '/api/v1/order/userAddresses/user',
    }).then(({ status, body }) => {
      console.log(body);
      expect(status).to.eq(200);
      if (body.data.length == 0) {
        cy.get('input[placeholder="Enter Name"]').type('Shubham Dubey');
        cy.get('input[placeholder="Enter Zipcode"]').type('32545');
        cy.get('input[placeholder="Enter State"]').type('Dallas');
        cy.get('input[placeholder="Enter City"]').type('New york');
        cy.get('textarea[placeholder="Enter Address Line 1"]').type(
          'Street A, near block'
        );
        cy.get('#add-address-button').should('exist');
        cy.get('#add-address-button').click();
      }
    });
    cy.get('#pay-later-button').should('exist');
    cy.get('#pay-later-button').click();
    cy.get('#modal-title').contains('Confirm Order');
    cy.get('#yes-button').should('exist');

    cy.get('#yes-button')
      .click()
      .then(() => {
        cy.wait('@ordersCall', { timeout: 60000 })
          .its('response.body')
          .then((body) => {
            let orderAddress = body.data[0][1];
            if (orderAddress) {
              cy.contains('Order created successfully').should('be.visible');
              cy.get('#user-dropdown').click();
              cy.get('#logout').click();
              cy.get('#Orders').should('not.exist');

              cy.visit('/');
              cy.get('#Login').click();
              cy.loginAsSeller();

              cy.get('#Orders').should('exist');
              cy.visit(`/marketplace/sold-orders/${orderAddress}`);
              cy.url().should(
                `include`,
                `/marketplace/sold-orders/${orderAddress}`
              );

              cy.get('#upload-button').should('exist');
              cy.get('#upload-button').click();
              cy.get('.ant-upload').contains('Upload CSV').should('exist');
              cy.get('input[type="file"]').selectFile(
                'cypress/fixtures/sample_1.csv',
                { force: true }
              );
              cy.get('#confirm-button').should('exist');
              cy.get('#confirm-button').click();
              cy.contains('Item created successfully').should('be.visible');

              cy.get('textarea[placeholder="Enter Comments"]').type(
                'I want to close this order'
              );
              cy.get('.ant-picker-input').click();
              cy.get('.ant-picker-today-btn').click();
              cy.get('#save-button').should('exist');
              cy.get('#save-button').click();

              cy.contains('Order has been updated').should('be.visible');
            }
          });
      });
  });

  it('it should allow seller to change the order status to awaiting shipment', () => {
    cy.intercept({
      method: 'POST',
      url: '/api/v1/order',
    }).as('ordersCall');

    cy.intercept({
      method: 'GET',
      url: '/api/v1/product?isDeleted=false&category=Art&subCategory=Art',
    }).as('productNameCall');

    cy.visit('/');
    cy.get('#Login').click();
    cy.loginAsSeller();

    const productName = `Corn-Seeds-${dayjs().unix()}`;
    cy.createProduct(productName);
    cy.createInventory(productName);

    cy.get('#user-dropdown').click();
    cy.get('#logout').click();
    cy.get('#Orders').should('not.exist');

    cy.visit('/');
    cy.get('#Login').click();
    cy.login();

    cy.url().should('include', '/marketplace');
    cy.get('#Art').should('exist');
    cy.get('#Art').click();
    cy.get(`#${productName}-buy-now`).should('exist');
    cy.get(`#${productName}-buy-now`).click();
    cy.url().should('include', '/marketplace/checkout');
    cy.get('#submit-order-button').should('exist');
    cy.get('#submit-order-button').click();
    cy.url().should('include', '/marketplace/confirmOrder');

    cy.request({
      method: 'GET',
      url: '/api/v1/order/userAddresses/user',
    }).then(({ status, body }) => {
      console.log(body);
      expect(status).to.eq(200);
      if (body.data.length == 0) {
        cy.get('input[placeholder="Enter Name"]').type('Shubham Dubey');
        cy.get('input[placeholder="Enter Zipcode"]').type('32545');
        cy.get('input[placeholder="Enter State"]').type('Dallas');
        cy.get('input[placeholder="Enter City"]').type('New york');
        cy.get('textarea[placeholder="Enter Address Line 1"]').type(
          'Street A, near block'
        );
        cy.get('#add-address-button').should('exist');
        cy.get('#add-address-button').click();
      }
    });
    cy.get('#pay-later-button').should('exist');
    cy.get('#pay-later-button').click();
    cy.get('#modal-title').contains('Confirm Order');
    cy.get('#yes-button').should('exist');
    // cy.get("#yes-button").click();

    cy.get('#yes-button')
      .click()
      .then(() => {
        let orderAddress;
        cy.wait('@ordersCall', { timeout: 60000 })
          .its('response.body')
          .then((body) => {
            console.log(body);
            orderAddress = body.data[0][1];
            if (orderAddress) {
              cy.contains('Order created successfully').should('be.visible');
              cy.get('#user-dropdown').click();
              cy.get('#logout').click();
              cy.get('#Orders').should('not.exist');

              cy.visit('/');
              cy.get('#Login').click();
              cy.loginAsSeller();

              cy.get('#Orders').should('exist');
              cy.visit(`/marketplace/sold-orders/${orderAddress}`);

              cy.get('#upload-button').should('exist');
              cy.get('#upload-button').click();
              cy.get('.ant-upload').contains('Upload CSV').should('exist');
              cy.get('input[type="file"]').selectFile(
                'cypress/fixtures/sample_1.csv',
                { force: true }
              );
              cy.get('#confirm-button').should('exist');
              cy.get('#confirm-button').click();

              cy.contains('Item created successfully').should('be.visible');
            }
          });
      });
  });
});
